# Design: extending `cljr-change-function-signature`

Status: **proposed** (not yet implemented). Captures the plan for making
`cljr-change-function-signature` add and remove parameters, and handle
multi-arity functions.

## Current state

Today `cljr-change-function-signature` can only **reorder** and **rename**
parameters, and it refuses multi-arity functions.

- `cljr--get-function-params` reads `arglists-str` from `cljr--var-info` and
  hard-errors with "Can't do work on functions of multiple arities" unless the
  var has exactly one arglist.
- The edit buffer (`cljr--setup-change-signature-buffer`,
  `cljr--change-signature-mode-map`) supports only `M-n`/`M-p` (move a param)
  and `e` (rename a param).
- On commit, `cljr--change-function-signature` uses `find-symbol` to get every
  occurrence, then rewrites each one Emacs-side. Occurrences are classified by
  `cljr--update-call-site` / `cljr--update-apply-call-site` /
  `cljr--update-partial-call-site` / `cljr--update-function-signature`, and
  anything that can't be handled safely is appended to a
  manual-intervention buffer (`cljr--append-to-manual-intervention-buffer`).

The data model is a list of `signature-changes` hashmaps with keys
`:old-index`, `:new-index`, `:old-name`, `:new-name`. It encodes a **pure
permutation + rename**: `cljr--update-call-site` extracts exactly N arguments
and re-inserts them in the new order. Every part assumes the argument count is
constant.

## Goals

- Add a parameter (with a default/placeholder inserted at call sites).
- Remove a parameter.
- Support multi-arity `defn`s.

## Non-goals

- Type-aware or value-aware argument synthesis. Added arguments get a
  placeholder, not an inferred value.
- Cross-language (cljs/cljc `:cljs`) support beyond what `find-symbol`
  already provides.

## The core difficulty

Add/remove change the **arity** of every call site, which the current model
cannot express. And unlike reorder/rename, they change program behavior:

- **Add**: every call site needs a new argument inserted. There's no correct
  value to synthesize, so we insert a placeholder the user must fill.
- **Remove**: every call site loses an argument. Silently deleting an argument
  is dangerous (it may have side effects or be needed elsewhere), so removal
  should default to flagging call sites for review rather than auto-deleting.

Because the rewriting is **project-wide and destructive**, and only runs
against a live refactor-nrepl connection over a real project, this work needs
integration testing against a real project - it can't be validated by the
buttercup suite alone.

## Proposed design

### 1. Extend the `signature-changes` model

Add an operation tag so entries aren't only permutations. Each entry becomes
one of:

- `{:op :keep  :old-index i :new-index j :old-name n :new-name m}` - the
  existing reorder/rename behavior.
- `{:op :add   :new-index j :new-name m :default "nil"}` - a new parameter;
  `:default` is the placeholder inserted at call sites.
- `{:op :remove :old-index i :old-name n}` - a removed parameter.

Keep backward compatibility by treating an entry without `:op` as `:keep`.

### 2. Edit-buffer UI

Extend `cljr--change-signature-mode-map`:

- `a` - add a parameter row (prompt for name and default placeholder).
- `k` / `d` - mark the current row for removal (render it struck-through or
  prefixed, don't delete the row until commit so it can be undone).
- keep `M-n`/`M-p` (reorder), `e` (rename), `RET`/`C-c C-c` (commit),
  `q`/`C-c C-k` (abort).

`cljr--setup-change-signature-buffer` renders the current params; new rows and
removal marks are tracked so commit can build the tagged `signature-changes`.

### 3. Update the definition

`cljr--update-function-signature` inserts new param names into the lambda list
at `:new-index` and removes params at `:old-index`, in addition to the current
reorder/rename.

### 4. Update call sites

- **reorder/rename** (`:keep`): unchanged.
- **add**: insert the `:default` placeholder at `:new-index`. Because the whole
  operation is applied via `find-symbol`, collect the inserted placeholder
  locations so the user can jump between them afterwards (or at minimum, list
  them in a summary buffer).
- **remove**: **do not auto-delete.** Route every affected direct call site to
  the manual-intervention buffer, so the user reviews each deletion. (A future
  refinement could auto-delete only when the argument is a literal with no
  side effects, but that's out of scope for the first version.)
- `apply` and `partial` call sites (`cljr--update-apply-call-site`,
  `cljr--update-partial-call-site`): for add/remove, route to manual
  intervention whenever the change could affect the spliced/partially-applied
  args, mirroring how they already bail on ambiguous reorders.

### 5. Multi-arity (phase 2)

This is a redesign, not a small addition:

- `cljr--get-function-params` returns the list of arglists instead of erroring.
- The edit buffer edits each arity (or a chosen arity), producing per-arity
  `signature-changes`.
- Call sites are matched to an arity by argument count before applying that
  arity's changes; call sites whose arity is ambiguous (e.g. behind `apply`)
  go to manual intervention.

Phase 2 should land only after phase 1 (add/remove for single-arity) is solid.

## Risks and mitigations

- **Destructive, project-wide rewriting.** Mitigate by preferring the
  manual-intervention buffer over silent edits for removal and for any
  ambiguous site, and by keeping the existing "save each file, report a
  summary" flow. A project-wide preview/undo (see the roadmap's "preview +
  atomic undo" idea) would make this dramatically safer and should ideally
  land first or alongside.
- **Placeholder discoverability for adds.** After an add, the user has N
  unfilled placeholders scattered across the project. Provide a way to review
  them (a summary buffer, or reuse the manual-intervention buffer listing).
- **Behavior change.** Adds/removes change program behavior by construction;
  the command should say so clearly before applying.

## Testing strategy

- **buttercup** for the pure pieces: the tagged `signature-changes` builder,
  arglist-string parsing (incl. multi-arity), and the definition-update logic
  on in-buffer text.
- **ecukes** with the mocked middleware for the edit-buffer + apply flow, the
  way the existing change-signature scenarios work.
- **Manual, against a real project** for the call-site rewriting - this is the
  part that cannot be trusted from unit tests alone.

## Phasing

1. **P1**: single-arity add-param (placeholder at call sites) and remove-param
   (route to manual intervention), edit-buffer UI, tagged model, tests.
2. **P2**: multi-arity.
3. Ideally paired with a project-wide **preview + atomic undo** so destructive
   application is reviewable.

## Open questions

- Default placeholder for added args: fixed `nil`, a per-add prompt, or a
  configurable default?
- Should remove ever auto-delete (literals with no side effects), or always
  route to manual review?
- For multi-arity, edit all arities at once or one selected arity per run?
