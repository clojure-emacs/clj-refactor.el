# clj-refactor.el Roadmap (as of June, 2026)

A high-level roadmap for clj-refactor.el and its backend, refactor-nrepl. It
focuses on the directions we consider most important over the next year or so.

It's meant to give users a general idea about where things are headed, and
collaborators a list of high-impact tasks worth tackling. It's not a promise or
a release plan - priorities shift, and good PRs have a way of reordering
everything.

## Where we are

Let's be honest about the state of things. clj-refactor.el itself has been in
maintenance mode for a while - the last feature release predates a lot of the
modern Clojure tooling landscape, and recent commits have mostly been
janitorial (Emacs 28+ baseline, compat cleanup, typo fixes). refactor-nrepl,
the backend, is the healthier half and keeps getting regular releases.

Meanwhile, clojure-lsp has become the default refactoring engine for most
Clojurists. It's fast, it's robust, it works on broken code, and it handles
ClojureScript. We can't pretend that hasn't changed what clj-refactor needs to
be.

The good news: a lot of the "dumb" structural refactorings have already moved
into clojure-mode, where they belong. What's left in clj-refactor.el splits
cleanly into three buckets, and that split drives everything below.

## The three buckets

1. **Pure structural edits** (paredit wrappers, let manipulation, extract
   constant/def, threading). These mostly live in clojure-mode now. The
   remaining ones should follow.
2. **REPL-backed but analysis-free** (`cljr-slash`, `add-missing-libspec` /
   `resolve-missing`, `clean-ns`, dependency management). These are the
   high-impact, everyday features people actually want, and crucially they do
   **not** need the AST index. These are prime candidates to move into CIDER
   proper.
3. **AST-dependent** (`find-usages`, `rename-symbol`, `change-function-signature`,
   `extract-function`, `inline-symbol`, `project-clean`). These need real
   whole-project analysis, and that's where the brittleness lives.

## Direction

The broad plan, coordinated with the CIDER 2.0 effort (see CIDER #3356):

### Slim down clj-refactor.el

Apply the same module-by-module audit approach we've used in CIDER: behavior-
preserving cleanups on focused branches, lean on built-ins, deprecate public
names with `make-obsolete` rather than deleting them outright. Concretely:

- ~~Remove dead code and long-obsolete aliases (the 2.x-era threading/cycling
  aliases that moved to clojure-mode years ago).~~ Done for 4.0.
- ~~Drop defensive compat checks that the Emacs 28+ / hard-`cider`-dependency
  baseline made unnecessary.~~ Done for 4.0.
- ~~Make narrow, heavyweight dependencies (multiple-cursors, hydra, inflections)
  soft/optional.~~ Done for 4.0 - they were dropped outright.
- Grow the buttercup suite to cover the pure-structural commands (which need
  no REPL to test). Both suites (buttercup and the ecukes feature suite) run
  in CI these days; buttercup remains the target for new tests.
- Split the single large source file along its existing logical seams, which
  also cleanly separates the AST-dependent commands from the rest.
- Vet every command under `clojure-ts-mode`, now that CIDER treats it as a
  first-class citizen. The commands delegate a lot to clojure-mode functions,
  which mostly work in ts buffers but can fail silently where sexp movement
  differs (the offline ns sort was one such case, fixed for 4.0).
- Migrate the jack-in injection off the legacy `cider-jack-in-lein-plugins` /
  `cider-add-to-alist` API when CIDER offers a modern replacement; the legacy
  vars still work in CIDER 2.0 but won't live forever.

### Move high-impact, analysis-free features into CIDER

The features in bucket 2 are "taken for granted in other IDEs," and users
shouldn't need a separate package for them. The plan is to bundle the relevant
refactor-nrepl ops with CIDER, reimplement the commands as `cider-*`, and
deprecate the clj-refactor copies. `cljr-slash`, `add-missing-libspec`, and
`clean-ns` are the first targets - none of them need the AST index, so they can
move without dragging the slow analyzer along.

The idiosyncratic parts (e.g. the yasnippet-based ns/defn snippets) stay in
clj-refactor.el. They don't belong in CIDER core.

### Make the analysis fast and robust

This is the big one, and the reason clojure-lsp has been winning.

Today refactor-nrepl has no real index. It analyzes namespaces lazily, per
request, by actually `require`-ing and evaluating your code through
tools.analyzer.jvm. That means it runs your side effects (and your tests), it
serializes everything behind a global lock, it falls over with OutOfMemory on
large projects, it can't analyze a buffer that doesn't compile, and it can't do
ClojureScript at all.

clojure-lsp avoids all of this by not evaluating your code. It drives clj-kondo
as a library to get static analysis - var definitions and usages, locals with
identity links, keywords, the require graph - all with exact source
coordinates, and caches it incrementally on disk. That's precisely what
find-usages, rename, and clean-ns need.

The plan:

- Introduce a clj-kondo-backed indexing backend behind refactor-nrepl's
  existing query API. clj-kondo is a plain JVM library; no subprocess needed.
- Adopt an incremental, persisted cache (re-analyze only changed files, analyze
  jars once, write the cache asynchronously). Skip the storage-format detours
  clojure-lsp went through and serialize straight to a simple format.
- Validate parity against the current AST index, then make clj-kondo the
  default. Keep the runtime/tools.analyzer path only for the few operations
  that genuinely need macroexpansion or runtime values.
- This lands the analysis behind a clean abstraction, so AST, clj-kondo, and
  potentially clojure-lsp-as-a-library become interchangeable implementations.

The payoff: analysis that's fast, survives broken code, doesn't run your tests,
and finally supports ClojureScript and `.cljc`.

### Modernize the backend build

refactor-nrepl still builds with Leiningen plus mranderson source-rewriting,
which is real dev friction (CI can't even parallelize it). Moving to tools.deps
is worth doing as the analysis work settles.

## Sequencing

Roughly, and subject to change:

1. **Now:** the clj-refactor.el cleanup sweeps and the test-suite fix. Low risk,
   shrinks the surface, makes everything after it safer to change.
2. **Next:** the clj-kondo indexing backend. This is what reverses the "losing
   to clojure-lsp" story and unlocks ClojureScript.
3. **Ongoing:** migrating the analysis-free features into CIDER, which the
   indexing abstraction makes cleaner.

Again: not a promise, just where our heads are at. PRs welcome on any of it.
