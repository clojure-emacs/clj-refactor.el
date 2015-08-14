;;; clj-refactor.el --- A collection of clojure refactoring functions

;; Copyright © 2012-2015 Magnar Sveen
;; Copyright © 2014-2015 Magnar Sveen, Lars Andersen, Benedek Fazekas

;; Author: Magnar Sveen <magnars@gmail.com>
;;         Lars Andersen <expez@expez.com>
;;         Benedek Fazekas
;; Version: 1.1.0
;; Keywords: convenience, clojure, cider
;; Package-Requires: ((emacs "24.3") (s "1.8.0") (dash "2.4.0") (yasnippet "0.6.1") (paredit "24") (multiple-cursors "1.2.2") (cider "0.9.1") (edn "1.1.2") (inflections "2.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README.md at https://github.com/clojure-emacs/clj-refactor.el

;;; Code:

(require 'dash)
(require 's)
(require 'yasnippet)
(require 'paredit)
(require 'multiple-cursors-core)
(require 'clojure-mode)
(require 'cider)
(require 'edn)
(require 'sgml-mode)
(require 'inflections)

(defconst cljr-version "1.2.0-SNAPSHOT"
  "The current version of clojure-refactor")

(defcustom cljr-add-ns-to-blank-clj-files t
  "If t, automatically add a ns form to new .clj files."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-sort-comparator #'cljr--string-natural-comparator
  "The comparator function to use to sort ns declaration.
Set your own if you see fit. Comparator is called with two
elements of the sub section of the ns declaration, and should
return non-nil if the first element should sort before the
second.

The following functions are also provided for use with this:
`cljr--string-length-comparator', `cljr--semantic-comparator',
and `cljr--string-natural-comparator'"
  :group 'cljr
  :type 'function)

(defcustom cljr-auto-sort-ns t
  "If t, sort ns form after any command that changes it."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-magic-requires t
  "Whether to automatically require common namespaces when they are used.
These are the namespaces listed in `cljr-magic-require-namespaces'.

If this variable is `:prompt', typing the short form followed by
`\\[cljr-slash]' will ask if you want to add the corresponding require
statement to the ns form.
Any other non-nil value means to add the form without asking."
  :group 'cljr
  :type '(choice (const :tag "true" t)
                 (const :tag "prompt" :prompt)
                 (const :tag "false" nil)))

(defcustom cljr-magic-require-namespaces
  '(("io"   . "clojure.java.io")
    ("set"  . "clojure.set")
    ("str"  . "clojure.string")
    ("walk" . "clojure.walk")
    ("zip"  . "clojure.zip"))
  "Alist of aliases and namespaces used by `cljr-slash'."
  :type '(repeat (cons (string :tag "Short alias")
                       (string :tag "Full namespace")))
  :group 'cljr)

(defcustom cljr-use-metadata-for-privacy nil
  "If nil, `cljr-cycle-privacy' will use (defn- f []).
If t, it will use (defn ^:private f [])."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-project-clean-prompt t
  "If t, `cljr-project-clean' asks before doing anything.
If nil, the project clean functions are run without warning."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-project-clean-functions
  (list #'cljr-remove-unused-requires #'cljr-sort-ns)
  "List of functions called by `cljr-project-clean'.
These are called on all .clj files in the project."
  :group 'cljr
  :type '(repeat function))

(defcustom cljr-project-clean-sorts-project-dependencies nil
  "If t, `cljr-project-clean' sorts project dependencies."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-project-clean-exceptions '("dev/user.clj" "project.clj" "boot.clj")
  "A list of files that `cljr-project-clean' should avoid."
  :group 'cljr
  :type '(repeat string))

(defcustom cljr-debug-functions "println,pr,prn"
  "List of functions used for debug purposes.
Used in `cljr-remove-debug-fns' feature."
  :group 'cljr
  :type 'string)

(defcustom cljr-hotload-dependencies t
  "If t, newly added dependencies are also hotloaded into the repl.
This only applies to dependencies added by `cljr-add-project-dependency'."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-favor-private-functions t
  "If t, refactorings insert private function declarations."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-favor-prefix-notation t
  "If t, `cljr-clean-ns' favors prefix notation in the ns form."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-use-multiple-cursors t
  "If t, some refactorings use the `multiple-cursors' package.
This improves interactivity of the commands. If nil, those
refactorings will use regular prompts instead."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-auto-clean-ns t
  "If t, call `cljr-clean-ns' after commands that change the ns."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-populate-artifact-cache-on-startup t
  "If t, the middleware will eagerly populate the artifact cache.
This makes `cljr-add-project-dependency' as snappy as can be."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-eagerly-build-asts-on-startup t
  "If t, the middleware will eagerly populate the ast cache.
This makes `cljr-find-symbol' and `cljr-rename-symbol' as snappy
as can be."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-suppress-middleware-warnings nil
  "If t, no middleware warnings are printed to the repl."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-find-usages-ignore-analyzer-errors nil
  "If t, `cljr-find-usages' ignores namespaces that cannot be analyzed.
If any namespaces presents an analyzer error, it is skipped and
the command carries on looking for the given symbol in those
namespaces which can be analyzed.

If nil, `cljr-find-usages' won't run if there is a broken
namespace in the project."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-auto-eval-ns-form t
  "When true refactorings which change the ns form also trigger
  its re-evaluation.")

(defvar clj-refactor-map (make-sparse-keymap) "")

(defvar cljr--add-require-snippet
  "${1:[${2:${3:} :as ${4:${3:$(cljr--ns-name yas-text)}}}]}"
  "The snippet used in in `cljr-add-require-to-ns'")

(defun cljr--ns-name (ns)
  "Return the last name in a full NS."
  (replace-regexp-in-string ".*\\." "" ns))

(defvar cljr--add-use-snippet "[$1 :refer ${2:[$3]}]"
  "The snippet used in in `cljr-add-use-to-ns'.")

(defvar *cljr--noninteractive* nil
  "t, when our interactive functions are called programmatically.")

(defvar cljr--nrepl-ops
  '(
    "artifact-list"
    "artifact-versions"
    "clean-ns"
    "configure"
    "extract-definition"
    "find-debug-fns"
    "find-symbol"
    "find-unbound"
    "hotload-dependency"
    "rename-file-or-dir"
    "resolve-missing"
    "stubs-for-interface"
    "version"
    "warm-ast-cache"
    ))

(defvar cljr--debug-mode nil)

(defvar cljr--occurrences nil)
(defvar cljr--signature-changes nil)
(defvar cljr--change-signature-buffer "*cljr-change-signature*")
(defvar cljr--manual-intervention-buffer "*cljr--manual-intervention*")

;;; Buffer Local Declarations

;; tracking state of find-symbol buffer

(defvar-local cjr--occurrence-count 0 "Counts occurrences of found symbols")

(defvar-local cljr--num-syms -1 "Keeps track of overall number of symbol occurrences")

(defmacro cljr--update-file (filename &rest body)
  "If there is an open buffer for FILENAME, then change that.
Otherwise open the file and do the changes non-interactively."
  (declare (debug (form body))
           (indent 1))
  (let ((fn (make-symbol "filename"))
        (bf (make-symbol "buffer")))
    `(let* ((,fn ,filename)
            (,bf (get-file-buffer ,fn)))
       (if ,bf
           (progn
             (set-buffer ,bf)
             ,@body
             (save-buffer))
         (with-temp-file ,fn
           (insert-file-contents ,fn)
           (delay-mode-hooks
             (clojure-mode)
             ,@body))))))

(define-key clj-refactor-map [remap paredit-raise-sexp] 'cljr-raise-sexp)
(define-key clj-refactor-map [remap paredit-splice-sexp-killing-backward] 'cljr-splice-sexp-killing-backward)
(define-key clj-refactor-map [remap paredit-splice-sexp-killing-forward] 'cljr-splice-sexp-killing-forward)
(define-key clj-refactor-map (kbd "/") 'cljr-slash)

(defun cljr--use-multiple-cursors? ()
  (and cljr-use-multiple-cursors
       (not (bound-and-true-p evil-mode))))

(defun cljr--fix-special-modifier-combinations (key)
  (case key
    ("C-s-i" "s-TAB")
    ("C-s-m" "s-RET")
    (otherwise key)))

(defun cljr--key-pairs-with-modifier (modifier keys)
  (->> (string-to-list keys)
       (--map (cljr--fix-special-modifier-combinations
               (concat modifier (char-to-string it))))
       (s-join " ")
       (read-kbd-macro)))

(defun cljr--key-pairs-with-prefix (prefix keys)
  (read-kbd-macro (concat prefix " " keys)))

(defvar cljr--all-helpers
  '(("ai" . (cljr-add-import-to-ns "Add import to ns"))
    ("am" . (cljr-add-missing-libspec "Add missing libspec"))
    ("ap" . (cljr-add-project-dependency "Add project dependency"))
    ("ar" . (cljr-add-require-to-ns "Add require to ns"))
    ("as" . (cljr-add-stubs "Add stubs for the interface / protocol at point."))
    ("au" . (cljr-add-use-to-ns "Add use to ns"))
    ("cc" . (cljr-cycle-coll "Cycle coll"))
    ("ci" . (cljr-cycle-if "Cycle if"))
    ("cn" . (cljr-clean-ns "Clean ns"))
    ("cp" . (cljr-cycle-privacy "Cycle privacy"))
    ("cs" . (cljr-change-function-signature "Change function signature"))
    ("ct" . (cljr-cycle-thread "Cycle thread"))
    ("dk" . (cljr-destructure-keys "Destructure keys"))
    ("ec" . (cljr-extract-constant "Extract constant"))
    ("ef" . (cljr-extract-function "Extract function"))
    ("el" . (cljr-expand-let "Expand let"))
    ("fe" . (cljr-create-fn-from-example "Create function from example"))
    ("fu" . (cljr-find-usages "Find usages"))
    ("hd" . (cljr-hotload-dependency "Hotload dependency"))
    ("il" . (cljr-introduce-let "Introduce let"))
    ("is" . (cljr-inline-symbol "Inline symbol"))
    ("mf" . (cljr-move-form "Move form"))
    ("ml" . (cljr-move-to-let "Move to let"))
    ("pc" . (cljr-project-clean "Project clean"))
    ("pf" . (cljr-promote-function "Promote function"))
    ("rd" . (cljr-remove-debug-fns "Remove debug fns"))
    ("rf" . (cljr-rename-file-or-dir "Rename file-or-dir"))
    ("rl" . (cljr-remove-let "Remove let"))
    ("rr" . (cljr-remove-unused-requires "Remove unused requires"))
    ("rs" . (cljr-rename-symbol "Rename symbol"))
    ("ru" . (cljr-replace-use "Replace use"))
    ("sn" . (cljr-sort-ns "Sort ns"))
    ("sc" . (cljr-show-changelog "Show the project's changelog"))
    ("sp" . (cljr-sort-project-dependencies "Sort project dependencies"))
    ("sr" . (cljr-stop-referring "Stop referring"))
    ("tf" . (cljr-thread-first-all "Thread first all"))
    ("th" . (cljr-thread "Thread"))
    ("tl" . (cljr-thread-last-all "Thread last all"))
    ("ua" . (cljr-unwind-all "Unwind all"))
    ("up" . (cljr-update-project-dependencies "Update project dependencies"))
    ("uw" . (cljr-unwind "Unwind"))
    ("ad" . (cljr-add-declaration "Add declaration"))
    ("?" . (cljr-describe-refactoring))))

(defun cljr--add-keybindings (key-fn)
  "Build the keymap from the list of keys/functions in `cljr--all-helpers'."
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (define-key clj-refactor-map (funcall key-fn key) fn))))

;;;###autoload
(defun cljr-add-keybindings-with-prefix (prefix)
  "Bind keys in `cljr--all-helpers' under a PREFIX key."
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-prefix prefix)))

;;;###autoload
(defun cljr-add-keybindings-with-modifier (modifier)
  "Bind keys in `cljr--all-helpers' under a MODIFIER key."
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-modifier modifier)))

;; ------ utilities -----------

(defun cljr--extract-sexp ()
  (buffer-substring (point) (cljr--point-after 'paredit-forward)))

(defun cljr--delete-sexp ()
  (delete-region (point) (cljr--point-after 'paredit-forward)))

(defun cljr--delete-and-extract-sexp ()
  (let* ((beg (point))
         (end (cljr--point-after 'paredit-forward))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    contents))

(defun cljr--extract-sexp-as-list ()
  "Returns list of strings representing the elements of the SEXP at point."
  (save-excursion
    (let* ((beg (progn (paredit-backward-up)
                       (forward-char)
                       (point)))
           (end (1- (cljr--point-after 'paredit-forward-up)))
           sexp-elems)
      (while (/= (point) end)
        (paredit-forward)
        (push (s-trim (buffer-substring-no-properties beg (point))) sexp-elems)
        (setq beg (point)))
      (nreverse sexp-elems))))

(defun cljr--extract-region (beg end)
  (prog1
      (buffer-substring-no-properties beg end)
    (delete-region beg end)))

(defun cljr--comment-line? ()
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at "\\s-*;+")))

(defun cljr--delete-and-extract-sexp-with-nested-sexps ()
  "Returns list of strings representing the nested sexps if there is any.
   In case there are no nested sexp the list will have only one element.
   Not recursive, does not drill down into nested sexps
   inside the first level nested sexps."
  (let* ((beg (point))
         (sexp-start beg)
         (end (progn (paredit-forward)
                     (point)))
         nested)
    (paredit-backward)
    (when (looking-at "\\[\\|(")
      (paredit-forward-down))
    (while (/= sexp-start end)
      (paredit-move-forward)
      (push (s-trim (buffer-substring sexp-start (point))) nested)
      (setq sexp-start (point)))
    (delete-region beg end)
    (nreverse (cons (concat (nth 1 nested) (car nested)) (or (nthcdr 2 nested) '())))))

(defun cljr--search-forward-within-sexp (s &optional save-excursion)
  "Searches forward for S in the current sexp.

if SAVE-EXCURSION is T POINT does not move."
  (let ((bound (save-excursion (forward-list 1) (point))))
    (if save-excursion
        (save-excursion
          (search-forward s bound t))
      (search-forward s bound t))))

(defun cljr--goto-toplevel ()
  (paredit-backward-up (cljr--depth-at-point))
  (when (looking-back "#")
    (backward-char)))

(defun cljr--toplevel-p ()
  "T unless we're in an s-expression or string."
  (= (cljr--depth-at-point) 0))

(defun cljr--depth-at-point ()
  "Returns the depth in s-expressions, or strings, at point."
  (let ((depth (car (paredit-current-parse-state))))
    (if (paredit-in-string-p)
        (1+ depth)
      depth)))

(defun cljr--cleanup-whitespace (stuff)
  "Removes blank lines preceding `stuff' as well as trailing whitespace."
  (with-temp-buffer
    (insert stuff)
    (goto-char (point-min))
    (delete-blank-lines)
    (when (looking-at "[ \t]*$")
      (delete-region (point-at-bol) (point-at-eol)))
    (let ((delete-trailing-lines t))
      (delete-trailing-whitespace)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun cljr--delete-line ()
  "Deletes the current line without introducing whitespace
errors."
  (delete-region (point-at-bol) (line-end-position))
  (join-line)
  (paredit-forward-delete 1))

(defun cljr--looking-at-dependency-vector-p ()
  (looking-at "\\[[^[[:space:]]+[[:space:]]+\""))

(defun cljr--just-one-blank-line ()
  "Ensure there's only one blank line at POINT."
  (newline 2)
  (forward-line -1)
  (delete-blank-lines))

(defun cljr--point-after (&rest actions)
  "Returns POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun cljr--make-room-for-toplevel-form ()
  (cljr--goto-toplevel)
  (goto-char (point-at-bol))
  (open-line 2))

(defun cljr--new-toplevel-form (form)
  "Insert a new toplevel FORM before the form containing POINT."
  (cljr--make-room-for-toplevel-form)
  (insert form))

(defun cljr-show-changelog ()
  "Show the changelog for `clj-refactor'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-show-changelog"
  (interactive)
  (let* ((cljr (cadr (assoc 'clj-refactor package-alist)))
         (dir (package-desc-dir cljr)))
    (find-file (format "%s/CHANGELOG.md" dir))
    (when (fboundp 'markdown-mode)
      (markdown-mode))
    (view-mode 1)))

(defun cljr--goto-toplevel-forward ()
  "Move forward and up until we reach toplevel."
  (paredit-forward-up (cljr--depth-at-point)))

(defun cljr--indent-defun ()
  "Indent the toplevel form containing point"
  (indent-region (cljr--point-after 'cljr--goto-toplevel)
                 (cljr--point-after 'cljr--goto-toplevel-forward)))

(defun cljr--point-at-text-matching
    (regexp direction &optional bound noerror count)
  "Return the point after searching in DIRECTION for TEXT.

DIRECTION is either :forward or :backward.

the optional arguments are passed on the to search function.  See
e.g. `re-search-forward'"
  (save-excursion
    (cond
     ;; NOTE: non-optional direction is intentional because I think it
     ;; improves readability greatly at the call site
     ((eq direction :forward)
      (re-search-forward regexp bound noerror count))
     ((eq direction :backward)
      (re-search-backward regexp bound noerror count))
     (t (error "Only know how to search :forward or :backward, you asked for '%s'"
               direction)))))

(defun cljr--resolve-alias (alias)
  "Looks up ALIAS in the ns form."
  (save-excursion
    (cljr--goto-ns)
    (when (re-search-forward
           (format ":as\\s-+%s\\_>" (regexp-quote alias))
           (cljr--point-after 'paredit-forward)
           :noerror)
      (paredit-backward-up)
      (paredit-forward-down)
      (buffer-substring-no-properties (point)
                                      (cljr--point-after 'paredit-forward)))))

(defun cljr--point-for-anon-function ()
  "Returns the location of point if the point is currently placed
at the opening parentheses of an anonymous function."
  (cond
   ((looking-at "(fn \\(\\_<[^ ]+\\_>[[:space:]\n]+\\)?\\[")
    (point))
   ((save-excursion (backward-char) (looking-at "#("))
    (1- (point)))))

(defun cljr--goto-fn-definition ()
  (if (zerop (car (paredit-current-parse-state)))
      (error "Not inside a s-expression.")
    (let* ((pt-orig (point))
           (search-bound (cljr--point-after 'cljr--goto-toplevel))
           found-fn-p)
      (while (not found-fn-p)
        (paredit-backward-up)
        (-if-let (fn-beg (cljr--point-for-anon-function))
            (let ((fn-end (save-excursion (paredit-forward) (point))))
              (when (and (< fn-beg pt-orig) (< pt-orig fn-end))
                (setq found-fn-p t)
                (when (looking-back "#")
                  (backward-char))))
          (when (<= (point) search-bound)
            (error "Can't find definition of anonymous function!")))))))

;; ------ reify protocol defrecord -----------

(defun cljr--goto-reify ()
  (let ((point (point)))
    (while (not (or (cljr--toplevel-p)
                    (looking-at-p "(reify")))
      (paredit-backward-up))
    (unless (looking-at-p "(reify")
      (goto-char point)
      (error "Can't find call to reify!"))))

(defun cljr-reify-to-defrecord ()
  "Replace a call to reify with a call to a new constructor.
A new record is created to define this constructor."
  (interactive "")
  (cljr--goto-reify)
  (let ((record-name (cljr--prompt-user-for "Name of new record: "))
        (reify-sexp (cljr--delete-and-extract-sexp))
        (placeholder "#85dffa31d"))
    (insert placeholder)
    (cljr--new-toplevel-form reify-sexp)
    (paredit-backward)
    (paredit-forward-down)
    (cljr--delete-and-extract-sexp)
    (insert "defrecord " record-name " []")
    (if (looking-at-p "[ \t]*$")
        (forward-line)
      (newline-and-indent))
    (cljr--goto-toplevel)
    (indent-region (point) (cljr--point-after 'paredit-forward))
    (re-search-forward placeholder)
    (paredit-backward)
    (cljr--delete-and-extract-sexp)
    (insert "("record-name ".)")
    (paredit-backward-down)))

;; ------ file -----------

(defun cljr--project-dir ()
  (or (ignore-errors
        (file-truename
         (locate-dominating-file default-directory "project.clj")))
      (ignore-errors
        (file-truename
         (locate-dominating-file default-directory "boot.clj")))
      (ignore-errors (file-truename
                      (locate-dominating-file default-directory "pom.xml")))))

(defun cljr--project-file ()
  (or (ignore-errors
        (expand-file-name "project.clj" (cljr--project-dir)))
      (ignore-errors
        (expand-file-name "boot.clj" (cljr--project-dir)))
      (ignore-errors (expand-file-name "pom.xml" (cljr--project-dir)))))

(defun cljr--project-files ()
  (split-string (shell-command-to-string
                 (format "find %s -type f \\( %s -or %s \\) %s | head -n %s"
                         (cljr--project-dir)
                         (format "-name \"%s\"" "*.clj")
                         (format "-name \"%s\"" "*.cljc")
                         "-not -regex \".*svn.*\""
                         1000))))

(defun cljr--indent-ns-forms (changed-files)
  "The middleware just dumps the new ns into the file and it's up
to us to make sure it's nicely indented."
  (save-window-excursion
    (dolist (f changed-files)
      (delay-mode-hooks
        (with-current-buffer (find-file-noselect f :nowarn)
          (revert-buffer :ignore-auto :no-confirm)
          (clojure-mode)
          (indent-region (point-min) (cljr--point-after 'paredit-forward))
          (save-buffer))))))

(defun cljr--buffers-visiting-dir (dir)
  (-filter (lambda (buf)
             (-when-let (path (buffer-file-name buf))
               (s-starts-with? dir path :ignore-case)))
           (buffer-list)))

(defun cljr--revisit-buffers (buffers new-dir active)
  "After moving a directory revisit all files visited by BUFFERS
  by looking them up in NEW-DIR.

ACTIVE is the buffer the user was looking at when the command was
issued, and should be left focused."
  (let ((files (directory-files new-dir))
        (new-dir (if (s-ends-with? "/" new-dir) new-dir (format "%s/" new-dir)))
        (same-file (lambda (buf f)
                     (when (string= (file-name-nondirectory f)
                                    (file-name-nondirectory (buffer-file-name buf)))
                       f))))
    (dolist (buf buffers)
      (find-file
       (format "%s%s" new-dir (-some (-partial same-file buf) files)))
      (kill-buffer buf))
    (find-file (format "%s/%s" new-dir (-some (-partial same-file active) files)))))

;;;###autoload
(defun cljr-rename-file-or-dir (old-path new-path)
  "Rename a file or directory of files.
Buffers visiting any affected file are killed and the
corresponding files are revisited.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-file-or-dir"
  (interactive
   (let ((old (read-file-name "Old path: " nil nil 'mustmatch "")))
     (list old
           (if (file-directory-p old)
               (read-directory-name "New path: " old)
             (read-file-name "New path: "
                             (file-name-directory old)
                             nil nil
                             (file-name-nondirectory old))))))
  (let* ((active-buffer (current-buffer))
         (affected-buffers (when (file-directory-p old-path)
                             (cljr--buffers-visiting-dir old-path)))
         (old-path (expand-file-name old-path))
         (new-path (expand-file-name new-path)))
    (when (y-or-n-p (format "Really rename %s to %s?" old-path new-path))
      (let* ((changed-files (cljr--call-middleware-sync (list "op" "rename-file-or-dir"
                                                              "old-path" old-path
                                                              "new-path" new-path)
                                                        "touched"))
             (changed-files-count (length changed-files)))
        (cljr--indent-ns-forms changed-files)
        (cond
         ((null changed-files) (message "Rename complete! No files affected."))
         ((= changed-files-count 1) (message "Renamed %s to %s." old-path new-path))
         (t (message "Rename complete! %s files affected." changed-files-count)))
        (when (> changed-files-count 0)
          (cljr--warm-ast-cache)))
      (if affected-buffers
          (cljr--revisit-buffers affected-buffers new-path active-buffer)
        (kill-buffer active-buffer)
        (find-file new-path)))))

;; ------ ns statements -----------

(defun cljr--goto-ns ()
  (goto-char (point-min))
  (if (re-search-forward clojure-namespace-name-regex nil t)
      (cljr--goto-toplevel)
    (error "No namespace declaration found")))

(defun cljr--insert-in-ns (type)
  (cljr--goto-ns)
  (if (cljr--search-forward-within-sexp (concat "(" type))
      (if (looking-at " *)")
          (progn
            (search-backward "(")
            (forward-list 1)
            (forward-char -1)
            (insert " "))
        (search-backward "(")
        (forward-list 1)
        (forward-char -1)
        (newline-and-indent))
    (forward-list 1)
    (forward-char -1)
    (newline-and-indent)
    (insert "(" type " )")
    (forward-char -1)))

(defun cljr--project-depends-on-p (package)
  (with-current-buffer
      (find-file-noselect (cljr--project-file))
    (goto-char (point-min))
    (search-forward package nil t)))

(defun cljr--find-source-ns-of-test-ns (test-ns test-file)
  (let* ((ns-chunks (split-string test-ns "[.]" t))
         (test-name (car (last ns-chunks)))
         (src-dir-name (s-replace "test/" "src/" (file-name-directory test-file)))
         (replace-underscore (-partial 's-replace "_" "-"))
         (src-ns (car (--filter (or (s-prefix-p it test-name)
                                    (s-suffix-p it test-name))
                                (-map (lambda (file-name)
                                        (funcall replace-underscore
                                                 (file-name-sans-extension file-name)))
                                      (directory-files src-dir-name))))))
    (when src-ns
      (mapconcat 'identity (append (butlast ns-chunks) (list src-ns)) "."))))

(defun cljr--add-test-use-declarations ()
  (save-excursion
    (let* ((ns (clojure-find-ns))
           (source-ns (cljr--find-source-ns-of-test-ns ns (buffer-file-name))))
      (cljr--insert-in-ns ":require")
      (when source-ns
        (insert "[" source-ns " :refer :all]"))
      (cljr--insert-in-ns ":require")
      (insert "[" (cond
                   ((cljr--project-depends-on-p "midje") "midje.sweet")
                   ((cljr--project-depends-on-p "expectations") "expectations")
                   (t "clojure.test"))
              " :refer :all]"))))

(defun cljr--in-tests-p ()
  "Check whether the current file is a test file.

Two checks are made - whether the namespace of the file has the
word test in it and whether the file lives under the test/ directory."
  (or (string-match-p "test\." (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))

(defun cljr--clojure-ish-filename-p (file-name)
  (or (s-ends-with? ".clj" file-name)
      (s-ends-with? ".cljs" file-name)
      (s-ends-with? ".cljx" file-name)
      (s-ends-with? ".cljc" file-name)))

(defun cljr--clojure-filename-p (file-name)
  (or (s-ends-with? ".clj" (buffer-file-name))
      (s-ends-with? ".cljc" (buffer-file-name))))

(defun cljr--add-ns-if-blank-clj-file ()
  (ignore-errors
    (when (and cljr-add-ns-to-blank-clj-files
               (cljr--clojure-ish-filename-p (buffer-file-name))
               (= (point-min) (point-max)))
      (clojure-insert-ns-form)
      (newline 2)
      (when (cljr--in-tests-p)
        (cljr--add-test-use-declarations)))))

(add-hook 'find-file-hook 'cljr--add-ns-if-blank-clj-file)

(defun cljr--verify-underscores-in-filename ()
  (let ((file-name (buffer-file-name)))
    (when (and
           file-name
           (not (file-exists-p file-name)) ;; only new files
           (s-matches? "-[^/]+\.clj[sxc]?$" file-name)
           (yes-or-no-p "The file name contains dashes. Replace with underscores?"))
      (let ((new-name (concat
                       (file-name-directory file-name)
                       (s-replace "-" "_" (file-name-nondirectory file-name)))))
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (message "Changed file name to '%s'"
                 (file-name-nondirectory new-name))))))

(add-hook 'find-file-hook 'cljr--verify-underscores-in-filename)

(defun cljr--extract-ns-statements (statement-type with-nested)
  (cljr--goto-ns)
  (if (or (not (cljr--search-forward-within-sexp (concat "(" statement-type)))
          (cljr--comment-line?))
      '()
    (let (statements)
      (while (not (looking-at " *)"))
        (push (if with-nested
                  (cljr--delete-and-extract-sexp-with-nested-sexps)
                (cljr--delete-and-extract-sexp)) statements))
      statements)))

(defun cljr--get-ns-statements (statement-type)
  (save-excursion
    (cljr--goto-ns)
    (when (cljr--search-forward-within-sexp (concat "(" statement-type))
      (let ((beg (point)))
        (paredit-forward-up)
        (paredit-backward-down)
        (buffer-substring-no-properties beg (point))))))

(defun cljr--only-alpha-chars (s)
  (replace-regexp-in-string "[^[:alnum:]]" "" s))

(defun cljr--string-natural-comparator (s1 s2)
  (string< (cljr--only-alpha-chars s1)
           (cljr--only-alpha-chars s2)))

(defun cljr--string-length-comparator (s1 s2)
  (> (length s1)
     (length s2)))

(defun cljr--semantic-comparator (ns s1 s2)
  "Sorts used, required namespaces closer to the ns of the current buffer
   before the rest.
   When above is not applicable falls back to natural comparator."
  (let ((shared-length-s1
         (length (s-shared-start ns (cljr--extract-sexp-content s1))))
        (shared-length-s2
         (length (s-shared-start ns (cljr--extract-sexp-content s2)))))
    (if (/= shared-length-s1 shared-length-s2)
        (> shared-length-s1 shared-length-s2)
      (cljr--string-natural-comparator s1 s2))))

(defun cljr-create-comparator (comparator-fn)
  (if (eq comparator-fn 'cljr--semantic-comparator)
      (-partial 'cljr--semantic-comparator (clojure-find-ns))
    comparator-fn))

;;;###autoload
(defun cljr-sort-ns ()
  "Sort the `ns' form according to `cljr-sort-comparator'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-sort-ns"
  (interactive)
  (save-excursion
    (let ((buf-already-modified? (buffer-modified-p))
          (comparator (cljr-create-comparator cljr-sort-comparator)))
      (dolist (statement-type '(":require" ":use" ":import"))
        (ignore-errors
          (let* ((statement        (->> (cljr--extract-ns-statements statement-type nil)
                                        (nreverse)
                                        (-map 's-trim)))
                 (sorted-statement (->> statement
                                        (-sort comparator)
                                        (-distinct))))
            (dolist (it sorted-statement)
              (cljr--insert-in-ns statement-type)
              (insert it))
            (when (and (not buf-already-modified?)
                       (buffer-modified-p)
                       (->> (-interleave statement sorted-statement)
                            (-partition 2)
                            (--map (apply 's-equals? it))
                            (--every? (eq it t))))
              (not-modified))))))))

(defun cljr--is-require-flag (req-statement)
  (let ((t-req (s-trim req-statement)))
    (or (string= t-req ":reload")
        (string= t-req ":reload-all")
        (string= t-req ":verbose"))))

(defun cljr--req-element-regexp (refered postfix)
  (concat "^[[:space:]]*[^;]*"
          "[^[:word:]^-]"
          (regexp-quote refered)
          postfix))

(defun cljr--extract-sexp-content (sexp)
  (replace-regexp-in-string "\\[?(?]?)?" "" sexp))

(defun cljr--is-name-in-use-p (name)
  (goto-char (point-min))
  (let ((e (cljr--extract-sexp-content name)))
    (when (re-search-forward (cljr--req-element-regexp e "[^[:word:]^-]") nil t) e)))

(defun cljr-remove-debug-fns ()
  "Remove from current buffer all calls to debugging functions.
These are functions listed in `cljr-debug-functions'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-remove-debug-fns"
  (interactive)
  (cljr--assert-middleware)
  (let* ((body (replace-regexp-in-string "\"" "\"" (buffer-substring-no-properties (point-min) (point-max))))
         (result (cljr--call-middleware-sync
                  (list "op" "find-debug-fns"
                        "ns-string" body
                        "debug-fns" cljr-debug-functions)
                  "value"))
         (debug-fn-tuples (pop result))
         (removed-lines 0))
    (while debug-fn-tuples
      (let ((line (- (1- (car debug-fn-tuples)) removed-lines))
            (end-line (nth 1 debug-fn-tuples))
            (column (nth 2 debug-fn-tuples)))
        (message "removing %s at line %s [%s] column %s (end-line %s end-column %s)"
                 (-last-item debug-fn-tuples) line (car debug-fn-tuples) column end-line (nth 3 debug-fn-tuples))
        (save-excursion
          (goto-char (point-min))
          (forward-line line)
          (move-to-column column)
          (paredit-backward)
          (cljr--delete-and-extract-sexp)
          (join-line))
        (setq removed-lines (+ removed-lines (1+ (- end-line (car debug-fn-tuples))))))
      (setq debug-fn-tuples (pop result)))))


(defun cljr--rectify-refer-type-require (sexp-as-list refer-index as-used as-index)
  (let* ((as-after-refer (and as-used (> as-index refer-index)))
         (sexp-wo-as (if as-after-refer
                         (-take as-index sexp-as-list)
                       sexp-as-list))
         (referred-names (->> sexp-wo-as
                              (nthcdr (1+ refer-index))
                              (-map 'cljr--is-name-in-use-p)
                              (delq nil))))
    (cond (referred-names
           (format "%s [%s]%s"
                   (s-join " " (if (and as-used (< as-index refer-index))
                                   (-take (1+ refer-index) sexp-as-list)
                                 (list (replace-regexp-in-string "(" "[" (car sexp-as-list)) ":refer")))
                   (s-join " " referred-names)
                   (if as-after-refer
                       (concat " " (s-join " " (list ":as" (nth (1+ as-index) sexp-as-list))))
                     "]")))
          (as-used
           (format "%s]" (s-join " " (list (car sexp-as-list)
                                           (nth as-index sexp-as-list)
                                           (cljr--extract-sexp-content (nth (1+ as-index) sexp-as-list)))))))))

(defun cljr--is-simple-req-statement-in-use (sexp as-list alias-used refer-used)
  (or (s-match ":refer[[:space:]]+:all" sexp)
      (cljr--is-require-flag (cljr--extract-sexp-content sexp))
      (and (= 1 (safe-length as-list))
           (re-search-forward (cljr--req-element-regexp (cljr--extract-sexp-content (car as-list)) "/") nil t))
      (and alias-used (not refer-used))))

(defun cljr--rectify-simple-req-statement (req sexp-as-list)
  (save-excursion
    (goto-char (point-min))
    (let* ((refer-index (-elem-index ":refer" sexp-as-list))
           (as-index (-elem-index ":as" sexp-as-list))
           (as-used (and as-index
                         (re-search-forward (cljr--req-element-regexp (cljr--extract-sexp-content (nth (1+ as-index) sexp-as-list)) "/") nil t))))
      (cond ((cljr--is-simple-req-statement-in-use req sexp-as-list as-used refer-index) req)
            (refer-index
             (cljr--rectify-refer-type-require sexp-as-list refer-index as-used as-index))))))

(defun cljr--is-prefix-element-in-use (f-elem p-elem)
  (goto-char (point-min))
  (let ((elem (replace-regexp-in-string "]]]?" "]" p-elem)))
    (if (s-matches? "^\\[\\|(" elem)
        (let ((result (cljr--rectify-simple-req-statement elem (split-string elem))))
          (when result (concat "\n" result)))
      (when (re-search-forward (cljr--req-element-regexp (s-join "." (list f-elem (cljr--extract-sexp-content elem))) "/") nil t) (cljr--extract-sexp-content elem)))))

(defun cljr--rectify-prefix-list-req-statement (require-as-list)
  (let* ((first-element (cljr--extract-sexp-content (car require-as-list)))
         (used-elements (->> require-as-list
                             (nthcdr 1)
                             (-map (apply-partially 'cljr--is-prefix-element-in-use first-element))
                             (delq nil))))
    (when used-elements
      (format "[%s %s]" first-element (s-join " " used-elements)))))

(defun cljr--rectify-req-statement (require-as-list)
  (save-excursion
    (let ((sexp-as-list (-flatten (-map (lambda (sexp) (split-string sexp)) require-as-list))))
      (if (or (= 1 (safe-length sexp-as-list))
              (string= ":refer" (nth 1 sexp-as-list))
              (string= ":as" (nth 1 sexp-as-list)))
          (cljr--rectify-simple-req-statement (s-join " " require-as-list) sexp-as-list)
        (cljr--rectify-prefix-list-req-statement require-as-list)))))

(defun cljr--remove-require ()
  (search-backward "(")
  (cljr--delete-and-extract-sexp)
  (join-line))

(defun cljr--req-statement-is-for-current-ns (s)
  (string= (cljr--current-namespace)
           (car (split-string (cljr--extract-sexp-content (s-join " " s))))))

(defun cljr--maybe-sort-ns ()
  (when cljr-auto-sort-ns
    (cljr-sort-ns)))

(defun cljr--maybe-clean-or-sort-ns ()
  (if (and cljr-auto-clean-ns (cider-connected-p)
           (nrepl-op-supported-p "clean-ns"))
      (cljr-clean-ns)
    (when cljr-auto-sort-ns
      (cljr-sort-ns))))

;;;###autoload
(defun cljr-remove-unused-requires ()
  "Remove from the ns form any requires not being used.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-remove-unused-requires"
  (interactive)
  (save-excursion
    (let (req-exists)
      (dolist (statement (->> (cljr--extract-ns-statements ":require" t)
                              (-remove 'cljr--req-statement-is-for-current-ns)
                              (-map 'cljr--rectify-req-statement)
                              (delq nil)
                              (nreverse)))
        (cljr--insert-in-ns ":require")
        (insert statement)
        (setq req-exists t))
      (when (not req-exists) (cljr--remove-require)))
    (paredit-backward-up)
    (let ((beg (point))
          (end (progn (paredit-forward) (point))))
      (delete-trailing-whitespace beg end)
      (indent-region beg end))))

(defvar cljr--tmp-marker (make-marker))

(defun cljr--pop-tmp-marker-after-yasnippet-1 (&rest ignore)
  (goto-char cljr--tmp-marker)
  (set-marker cljr--tmp-marker nil)
  (remove-hook 'yas/after-exit-snippet-hook
               'cljr--pop-tmp-marker-after-yasnippet-1 :local))

(defun cljr--pop-tmp-marker-after-yasnippet ()
  (add-hook 'yas/after-exit-snippet-hook
            'cljr--pop-tmp-marker-after-yasnippet-1 nil :local))

(defun cljr--sort-and-remove-hook (&rest ignore)
  (when cljr-auto-sort-ns
    (cljr-sort-ns))
  (remove-hook 'yas/after-exit-snippet-hook
               'cljr--sort-and-remove-hook :local))

(defun cljr--maybe-eval-ns-form-and-remove-hook ()
  (cljr--maybe-eval-ns-form)
  (remove-hook 'yas/after-exit-snippet-hook
               'cljr--maybe-eval-ns-form-and-remove-hook :local))

(defun cljr--add-yas-ns-updated-hook ()
  (add-hook 'yas/after-exit-snippet-hook 'cljr--sort-and-remove-hook nil :local)
  (add-hook 'yas/after-exit-snippet-hook
            'cljr--maybe-eval-ns-form-and-remove-hook nil :local))

;;;###autoload
(defun cljr-add-require-to-ns ()
  "Add a require statement to the ns form in current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-require-to-ns"
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require")
  (cljr--pop-tmp-marker-after-yasnippet)
  (cljr--add-yas-ns-updated-hook)
  (yas-expand-snippet cljr--add-require-snippet))

;;;###autoload
(defun cljr-add-use-to-ns ()
  "Add a use statement to the buffer's ns form.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-use-to-ns"
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require")
  (cljr--pop-tmp-marker-after-yasnippet)
  (cljr--add-yas-ns-updated-hook)
  (yas-expand-snippet cljr--add-use-snippet))

;;;###autoload
(defun cljr-add-import-to-ns ()
  "Add an import statement to the buffer's ns form.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-import-to-ns"
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":import")
  (cljr--pop-tmp-marker-after-yasnippet)
  (cljr--add-yas-ns-updated-hook)
  (yas-expand-snippet "$1"))

(defun cljr--extract-ns-from-use ()
  "Let point be denoted by |.  Then, when called on: |[used.ns ...]
returns used.ns, when called on (:use some.ns) returns some.ns"
  (let* ((form (format "%s" (sexp-at-point))))
    (if (looking-at "(:use [A-z.0-9-]+)")
        (s-chop-suffix ")" (cadr (s-split " " form)))
      (substring form 1 (min (or (s-index-of " " form) (1- (length form))
                                 (1- (length form))))))))

(defun cljr--extract-multiple-ns-from-use ()
  "Let point be denoted by |.  Then, when called on: |[used.ns lib1 lib2]
returns (used.ns.lib1 used.ns.lib2)"
  (let* ((form (format "%s" (sexp-at-point)))
         (form (substring form 1 (1- (length form))))
         (words (s-split " " form))
         (prefix (pop words))
         (libs (nreverse words)))
    (-map (lambda (lib) (concat prefix "." lib)) libs)))

(defun cljr--multiple-namespaces-p (use-form)
  "Returns t if the use form looks like [some.lib ns1 ns2 ...]"
  (unless (s-contains? ":only" (format "%s" use-form))
    (s-matches-p "\\[[A-z0-9.]+ \\(\\([A-z0-9]+ \\)\\|\\([A-z0-9]+\\)\\)+\\]"
                 (format "%s" use-form))))

(defun cljr--more-namespaces-in-use-p (nth)
  "Checks for, and moves POINT to, the NTH :use clause."
  (cljr--goto-ns)
  (cljr--search-forward-within-sexp "(:use ")
  (paredit-backward-up)
  (let ((use-end (save-excursion (forward-sexp) (point))))
    (prog1
        (re-search-forward "\\(\\(\\( \\)\\{2,\\}\\|:use \\)\\(\\[\\(.\\|\n\\)*?\\]\\)\\)\\|\\((:use [^]]+?)\\)" use-end t nth)
      (if (and (looking-back "\\]") (looking-at "\\]"))
          (paredit-backward-up)
        (paredit-backward)))))

(defun cljr--extract-used-namespaces ()
  "Return list of all the namespaces that are :used."
  (let (libs use-start next-use-clause)
    (cljr--goto-ns)
    (if (not (cljr--search-forward-within-sexp "(:use "))
        (message "There is no :use clause in the ns declaration.")
      (save-excursion
        (paredit-backward-up)
        (paredit-forward))
      (let ((next-use-clause 1))
        (while (cljr--more-namespaces-in-use-p next-use-clause)
          (push (if (cljr--multiple-namespaces-p (sexp-at-point))
                    (cljr--extract-multiple-ns-from-use)
                  (cljr--extract-ns-from-use))
                libs)
          (setq next-use-clause (1+ next-use-clause)))
        (nreverse (-flatten libs))))))

;;;###autoload
(defun cljr-replace-use ()
  "Replace any :use clause with the equivalent :require clause.

Presently, there's no support for :use clauses containing :exclude.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-replace-use"
  (interactive)
  (save-excursion
    (dolist (used-ns (cljr--extract-used-namespaces))
      (cljr--goto-ns)
      (cljr--search-forward-within-sexp used-ns)
      (if (ignore-errors (cljr--search-forward-within-sexp ":only"))
          (progn
            (paredit-forward-down)
            (let ((names (buffer-substring-no-properties (point)
                                                         (progn
                                                           (paredit-forward-up)
                                                           (1- (point))))))
              (cljr--insert-in-ns ":require")
              (insert (format "[%s :refer [%s]]" used-ns names))))
        (cljr--insert-in-ns ":require")
        (insert (format "[%s :refer :all]" used-ns))))
    (cljr--goto-ns)
    (cljr--search-forward-within-sexp ":use")
    (paredit-backward-up)
    (cljr--delete-and-extract-sexp)
    (join-line)
    (when (looking-at " ")
      (delete-char 1))
    (cljr--goto-ns)
    (paredit-forward)
    (indent-region (point-min) (point)))
  (cljr--maybe-clean-or-sort-ns))

;;;###autoload
(defun cljr-stop-referring ()
  "Stop referring to vars in the namespace at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-stop-referring"
  (interactive)
  (save-excursion
    (paredit-backward-up)
    (unless (looking-at "\\[")
      (error "Place cursor on the namespace whose vars you want to stop referring to."))
    (paredit-backward-up)
    (unless (looking-at "(:require ")
      (error "Place cursor on the namespace whose vars you want to stop referring to.")))
  (save-excursion
    (paredit-backward-up)
    (let* ((bound (save-excursion
                    (paredit-forward)
                    (point)))
           (ns (save-excursion
                 (paredit-forward-down)
                 (search-forward " :as " bound t)
                 (let ((beg (point)))
                   (paredit-forward)
                   (buffer-substring-no-properties beg (point))))))
      (unless (re-search-forward " :refer " bound t)
        (error "No :refer clause found."))
      (when (looking-at ":all")
        (error "Not smart enough to stop referring to :all unfortunately."))
      (paredit-forward-down)
      (let* ((beg (point))
             (str (progn (paredit-forward-up)
                         (paredit-backward-down)
                         (buffer-substring-no-properties beg (point))))
             (symbols (s-split " " (s-trim str) t)))
        (paredit-backward-up)
        (paredit-backward)
        (cljr--delete-and-extract-sexp)
        (cljr--delete-and-extract-sexp)
        (just-one-space 0)
        (cljr--add-ns-prefix ns symbols)))))

(defun cljr--add-ns-prefix (ns symbols)
  "Adds an NS prefix to every symbol in SYMBOLS."
  (save-excursion
    (cljr--goto-ns)
    (paredit-forward)
    (let ((case-fold-search nil))
      (while (re-search-forward (regexp-opt symbols 'symbols) nil t)
        (paredit-backward)
        (insert ns "/")
        (paredit-forward)))))

(defun cljr--insert-with-proper-whitespace (forms)
  (open-line 2)
  (forward-line 2)
  (let ((p (point)))
    (insert forms)
    (open-line 2)
    (forward-line)
    (cljr--just-one-blank-line)
    (save-excursion
      (goto-char p)
      (cljr--just-one-blank-line))))

;;;###autoload
(defun cljr-move-form ()
  "Move the form containing POINT to a new namespace.

If REGION is active, move all forms contained by region.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-form"
  (interactive)
  (let* ((forms (if (region-active-p)
                    (let ((beg (region-beginning))
                          (end (region-end)))
                      (prog2
                          (paredit-check-region-for-delete beg end)
                          (buffer-substring-no-properties beg end)
                        (delete-region beg end)))
                  (cljr--goto-toplevel)
                  (prog1 (cljr--delete-and-extract-sexp)
                    (join-line)
                    (join-line)
                    (delete-char 1))))
         (forms (cljr--cleanup-whitespace forms))
         (requires (cljr--get-ns-statements ":require")))
    (let (ns names)
      (save-window-excursion
        (ido-find-file)
        (goto-char (point-max))
        (cljr--insert-with-proper-whitespace forms)
        (when requires
          (cljr--insert-in-ns ":require")
          (insert requires)
          (cljr-remove-unused-requires))
        (save-buffer)
        (setq ns (cljr--current-namespace)
              names (cljr--name-of-defns forms)))
      (cljr--update-ns-after-moving-fns ns (nreverse names))
      (cljr-remove-unused-requires)))
  (cljr--just-one-blank-line))

(defun cljr--update-ns-after-moving-fns (ns &optional refer-names)
  "Updates the current ns declaration after moving defn forms out of the
  current file and to NS.  Optionally referring the names in REFER-NAMES."
  (save-excursion
    (cljr--goto-ns)
    (paredit-forward)
    (let* ((end-of-ns-form (prog1 (point) (paredit-backward)))
           (ns-present-p (cljr--search-forward-within-sexp ns :save-excursion))
           (refer-present-p (cljr--search-forward-within-sexp ":refer" :save-excursion))
           (refer-all-p (cljr--search-forward-within-sexp ":refer :all" :save-excursion))
           (require-present-p (cljr--search-forward-within-sexp
                               (s-concat ":require [" ns)
                               :save-excursion)))
      (if ns-present-p
          (unless (or refer-all-p (null refer-names))
            (if refer-present-p
                (cljr--append-names-to-refer ns refer-names)
              (when require-present-p
                (cljr--append-refer-clause ns refer-names))))
        (cljr--new-require-clause ns refer-names)))))

(defun cljr--append-refer-clause (ns refer-names)
  "Appends :refer [REFER-NAMES] to the :require clause for NS."
  (save-excursion
    (cljr--goto-ns)
    (re-search-forward ":require")
    (re-search-forward ns)
    (paredit-forward-up)
    (backward-char)
    (insert " :refer [" (s-join " " refer-names) "]")))

(defun cljr--append-names-to-refer (ns names)
  "Append NAMES to the :refer vector for NS"
  (save-excursion
    (cljr--goto-ns)
    (re-search-forward ":require")
    (re-search-forward ns)
    (re-search-forward ":refer")
    (paredit-forward)
    (backward-char)
    (apply #'insert " " (-interpose " " names))))

(defun cljr--new-require-clause (ns &optional refer-names)
  "Creates a new :require clause for NS.

Optionally adds :refer [REFER-NAMES] clause."
  (cljr--insert-in-ns ":require")
  (insert "[" ns "]")
  (when refer-names
    (cljr--append-refer-clause ns refer-names)))

(defmacro cljr--with-string-content (s &rest body)
  (declare (debug (form body))
           (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks
       (clojure-mode)
       (insert ,s)
       (goto-char (point-min))
       ,@body)))

(defun cljr--name-of-defns (string-with-defns &optional include-private)
  "Returns a list of the function names in STRING-WITH-DEFNS,
optionally including those that are declared private."
  (cljr--with-string-content string-with-defns
    (let ((count (paredit-count-sexps-forward))
          (names '()))
      (dotimes (_ count)
        (paredit-forward-down)
        (cljr--goto-toplevel)
        (forward-char)
        (if (and include-private (looking-at "defn-"))
            (push (cljr--name-of-current-def) names)
          (when (looking-at "defn ")
            (push (cljr--name-of-current-def) names)))
        (paredit-forward-up))
      names)))

(defun cljr--current-namespace ()
  (save-excursion
    (cljr--goto-ns)
    (forward-char)
    (paredit-forward)
    (forward-char)
    (let ((beg (point))
          (end (cljr--point-after 'paredit-forward)))
      (buffer-substring-no-properties beg end))))

;; ------ declare statements -----------

(defun cljr--goto-declare ()
  (goto-char (point-min))
  (if (re-search-forward "(declare" nil t)
      (paredit-forward-up)
    (cljr--goto-ns)
    (paredit-forward)
    (open-line 2)
    (forward-line 2)
    (insert "(declare)")))

(defun cljr--name-of-current-def ()
  (cljr--goto-toplevel)
  (ignore-errors (forward-char))
  (when (looking-at "def")
    (paredit-forward)
    (while (looking-at " ^")
      (paredit-forward))
    (forward-char)
    (let ((beg (point))
          (end (cljr--point-after 'paredit-forward)))
      (buffer-substring-no-properties beg end))))

(defun cljr--already-declared? (def)
  (save-excursion
    (cljr--goto-declare)
    (re-search-backward def (cljr--point-after 'paredit-backward) :no-error)))

(defun cljr--add-declaration (def)
  (cljr--goto-declare)
  (backward-char)
  (insert " " def)
  (message "Added declaration for %s" def))

;;;###autoload
(defun cljr-add-declaration ()
  "Add a declare for the current def near the top of the buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-declaration"
  (interactive)
  (save-excursion
    (-if-let (def (cljr--name-of-current-def))
        (if (cljr--already-declared? def)
            (message "%s is already declared" def)
          (cljr--add-declaration def))
      (message "Not inside a def form."))))

;; ------ extract constant ----------------

;;;###autoload
(defun cljr-extract-constant ()
  "Extract form at (or above) point as a constant.
Create a def for it at the top level, and replace its current
occurrence with the defined name.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-constant"
  (interactive)
  (let ((name (let ((highlight (cljr--highlight-sexp)))
                (unwind-protect
                    (cljr--prompt-user-for "Name: ")
                  (delete-overlay highlight))))
        (body (cljr--delete-and-extract-sexp))
        const-pos)
    (save-excursion
      (cljr--goto-ns)
      (paredit-forward)
      (insert "\n\n(def ")
      (insert name)
      (insert " " body ")")
      (setq const-pos (point)))
    (insert name)
    (save-excursion
      (query-replace body name nil const-pos (point-max)))))

;; ------ threading and unwinding -----------

(defun cljr--unwind-first ()
  (paredit-forward)
  (save-excursion
    (let ((contents (cljr--delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line -1))
      (cljr--ensure-parens-around-function-names)
      (paredit-forward-down)
      (paredit-forward)
      (insert contents)))
  (forward-char))

(defun cljr--ensure-parens-around-function-names ()
  (unless (looking-at "[\n\r\t ]?(")
    (skip-syntax-forward " ")
    (paredit-wrap-round)
    (paredit-backward-up)))

(defun cljr--unwind-last ()
  (paredit-forward)
  (save-excursion
    (let ((contents (cljr--delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line -1))
      (cljr--ensure-parens-around-function-names)
      (paredit-forward)
      (paredit-backward-down)
      (insert contents)))
  (forward-char))

(defun cljr--nothing-more-to-unwind ()
  (save-excursion
    (let ((beg (point)))
      (paredit-forward)
      (paredit-backward-down)
      (paredit-backward) ;; the last sexp
      (paredit-backward) ;; the threading macro
      (paredit-backward) ;; and the paren
      (= beg (point)))))

(defun cljr--pop-out-of-threading ()
  (paredit-forward-down)
  (paredit-forward)
  (paredit-raise-sexp))

(defun cljr--goto-thread ()
  (while (not (or (cljr--toplevel-p)
                  (looking-at "\(.*->>?[\n\r\t ]")))
    (paredit-backward-up)))

(defun cljr--reindent-thread ()
  (cljr--goto-thread)
  (let ((beg (point))
        (end (cljr--point-after 'paredit-forward)))
    (indent-region beg end)))

;;;###autoload
(defun cljr-cycle-thread ()
  "Cycle a threading macro between -> and ->>.
Also applies to other versions of the macros, like cond->.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-thread"
  (interactive)
  (save-excursion
    (cljr--goto-thread)
    (cond
     ((looking-at ".*->>")
      (paredit-forward-down)
      (paredit-forward)
      (backward-char)
      (delete-region (point) (+ 1 (point)))
      (cljr--reindent-thread))

     ((looking-at ".*->[^>]")
      (paredit-forward-down)
      (paredit-forward)
      (insert ">")
      (cljr--reindent-thread)))))

;;;###autoload
(defun cljr-unwind ()
  "Unwind thread at point or above point by one level.
Return nil if there are no more levels to unwind.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-unwind"
  (interactive)
  (ignore-errors
    (forward-char 3))
  (search-backward-regexp "([^-]*->")
  (if (cljr--nothing-more-to-unwind)
      (progn (cljr--pop-out-of-threading)
             nil)
    (paredit-forward-down)
    (cond
     ((looking-at "[^-]*->[\n\r\t ]")  (cljr--unwind-first))
     ((looking-at "[^-]*->>[\n\r\t ]") (cljr--unwind-last)))
    t))

;;;###autoload
(defun cljr-unwind-all ()
  "Fully unwind thread at point or above point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-unwind-all"
  (interactive)
  (while (cljr-unwind)
    t))

(defun cljr--remove-superfluous-parens ()
  (when (looking-at "([^ )]+)")
    (paredit-forward-down)
    (paredit-raise-sexp)))

(defun cljr--thread-first ()
  (paredit-forward-down)
  (paredit-forward)
  (let* ((beg (point))
         (end (progn (paredit-forward)
                     (point)))
         (contents (buffer-substring beg end)))
    (if (string= contents ")")
        (progn (message "Nothing more to thread.") nil)
      (delete-region beg end)
      (paredit-backward-up)
      (just-one-space 0)
      (insert contents)
      (newline-and-indent)
      (cljr--remove-superfluous-parens)
      t)))

(defun cljr--thread-last ()
  (paredit-forward 2)
  (paredit-backward-down)
  (let* ((end (point))
         (beg (progn (paredit-backward)
                     (point)))
         (contents (buffer-substring beg end)))
    (if (looking-back "(")
        (progn
          (message "Nothing more to thread.")
          nil)
      (delete-region beg end)
      (just-one-space 0)
      (paredit-backward-up)
      (insert contents)
      (newline-and-indent)
      (cljr--remove-superfluous-parens)
      t)))

(defun cljr--thread-guard ()
  (save-excursion
    (paredit-forward)
    (if (looking-at "[\n\r\t ]*(")
        t
      (message "Can only thread into lists.")
      nil)))

;;;###autoload
(defun cljr-thread ()
  "Thread by one more level an existing threading macro.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-thread"
  (interactive)
  (when (looking-at "(?[^-]*-?>")
    (goto-char (match-end 0)))
  (search-backward-regexp "([^-]*->")
  (paredit-forward-down)
  (if (not (cljr--thread-guard))
      nil
    (cond
     ((looking-at "[^-]*->[\n\r\t ]")  (cljr--thread-first))
     ((looking-at "[^-]*->>[\n\r\t ]") (cljr--thread-last)))))

;;;###autoload
(defun cljr-thread-first-all ()
  "Fully thread the form at point using ->.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-thread-first-all"
  (interactive)
  (save-excursion
    (paredit-wrap-round)
    (insert "-> "))
  (while (save-excursion (cljr-thread))
    t))

;;;###autoload
(defun cljr-thread-last-all ()
  "Fully thread the form at point using ->>.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-thread-last-all"
  (interactive)
  (save-excursion
    (paredit-wrap-round)
    (insert "->> "))
  (while (save-excursion (cljr-thread))
    t))

;; ------ let binding ----------

;;;###autoload
(defun cljr-introduce-let ()
  "Create a let form, binding the form at point.
The resulting let form can then be expanded with `\\[cljr-expand-let]'.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-introduce-let"
  (interactive)
  (paredit-wrap-round)
  (insert "let ")
  (paredit-wrap-square)
  (insert " ")
  (backward-char)
  (let ((name (unless (cljr--use-multiple-cursors?)
                (cljr--prompt-user-for "Name: "))))
    (if name
        (insert name)
      (mc/create-fake-cursor-at-point))
    (paredit-forward-up)
    (newline-and-indent)
    (if name
        (insert name)
      (mc/maybe-multiple-cursors-mode))))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-introduce-let)

(defun cljr--goto-let ()
  (let ((target-expr "\(\\(when-let\\|if-let\\|let\\)\\( \\|\\[\\)"))
    (while (not (or (cljr--toplevel-p)
                    (looking-at target-expr)))
      (paredit-backward-up))
    (looking-at target-expr)))

(defun cljr--get-let-bindings ()
  "Returns a list of lists. The inner lists contain two elements first is
   the binding, second is the init-expr"
  (cljr--goto-let)
  (paredit-forward-down 2)
  (paredit-backward)
  (let* ((start (point))
         (sexp-start start)
         (end (progn (paredit-forward)
                     (point)))
         bindings)
    (paredit-backward)
    (paredit-forward-down)
    (while (/= sexp-start end)
      (paredit-move-forward)
      (let ((sexp (buffer-substring-no-properties sexp-start (point))))
        (push (s-trim
               (if (= start sexp-start)
                   (substring sexp 1)
                 sexp))
              bindings))
      (setq sexp-start (point)))
    (-partition 2 (nreverse bindings))))

(defun cljr--sexp-regexp (sexp)
  (concat "\\([^[:word:]^-]\\)"
          (s-join "[[:space:]\n\r]+" (-map 'regexp-quote (s-split " " sexp t)))
          "\\([^[:word:]^-]\\)"))

(defun cljr--replace-sexp-with-binding (binding)
  (save-excursion
    (let ((bind-var (car binding))
          (init-expr (-last-item binding))
          (end (cljr--point-after 'cljr--goto-let 'paredit-forward)))
      (while (re-search-forward (cljr--sexp-regexp init-expr) end t)
        (replace-match (concat "\\1" bind-var "\\2"))))))

(defun cljr--one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

;;;###autoload
(defun cljr-expand-let ()
  "Expand the let form above point by one level.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-expand-let"
  (interactive)
  (multiple-cursors-mode 0)
  (cljr--goto-let)
  (paredit-forward-down 2)
  (paredit-forward-up)
  (cljr--skip-past-whitespace-and-comments)
  (paredit-convolute-sexp)
  (-each (cljr--get-let-bindings) 'cljr--replace-sexp-with-binding)
  (cljr--one-shot-keybinding "l" 'cljr-expand-let))

(defun cljr--replace-sexp-with-binding-in-let ()
  (remove-hook 'multiple-cursors-mode-disabled-hook 'cljr--replace-sexp-with-binding-in-let)
  (-each (cljr--get-let-bindings) 'cljr--replace-sexp-with-binding))

;;;###autoload
(defun cljr-move-to-let ()
  "Move the form at point to a binding in the nearest let.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-move-to-let"
  (interactive)
  (if (not (save-excursion (cljr--goto-let)))
      (cljr-introduce-let)
    (let ((name (unless (cljr--use-multiple-cursors?)
                  (cljr--prompt-user-for "Name: "))))
      (save-excursion
        (let ((contents (cljr--delete-and-extract-sexp)))
          (cljr--prepare-to-insert-new-let-binding)
          (insert contents))
        (backward-sexp)
        (insert " ")
        (backward-char)
        (if name
            (insert name)
          (mc/create-fake-cursor-at-point)))
      (if name
          (progn
            (insert name)
            (cljr--replace-sexp-with-binding-in-let))
        (add-hook 'multiple-cursors-mode-disabled-hook 'cljr--replace-sexp-with-binding-in-let)
        (mc/maybe-multiple-cursors-mode)))))

(defun cljr--prepare-to-insert-new-let-binding ()
  (if (cljr--inside-let-binding-form-p)
      (progn
        (paredit-backward-up (- (cljr--depth-at-point)
                                (cljr--depth-of-let-bindings)))
        (paredit-backward)
        (newline-and-indent)
        (previous-line)
        (indent-for-tab-command))
    (cljr--goto-let)
    (search-forward "[")
    (paredit-backward)
    (paredit-forward)
    (paredit-backward-down)
    (backward-char)
    (if (looking-at "\\[ *\\]")
        (forward-char)
      (forward-char)
      (newline-and-indent))))

(defun cljr--inside-let-binding-form-p ()
  (ignore-errors
    (save-excursion
      (let ((pos (point)))
        (cljr--goto-let)
        (re-search-forward "\\[")
        (if (< pos (point))
            nil
          (paredit-forward-up)
          (< pos (point)))))))

(defun cljr--depth-of-let-bindings ()
  "Returns the depth where the variable bindings for the active
let are."
  (save-excursion
    (cljr--goto-let)
    (re-search-forward "\\[")
    (cljr--depth-at-point)))

(defun cljr--eliminate-let ()
  "Remove a the nearest let form.

This function only does the actual removal."
  (cljr--goto-let)
  (paredit-forward-down)
  (paredit-forward 2)
  (paredit-splice-sexp-killing-backward))

(defun cljr-remove-let ()
  "Inlines all variables in the let form and removes it.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-remove-let"
  (interactive)
  (save-excursion
    (let ((*cljr--noninteractive* t)) ; make `cljr-inline-symbol' be quiet
      (cljr--goto-let)
      (paredit-forward-down 2)
      (dotimes (_ (length (save-excursion (cljr--get-let-bindings))))
        (cljr-inline-symbol)))))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-move-to-let)

;; ------ Destructuring ----

(defun cljr--find-symbol-at-point ()
  (save-excursion
    (when (looking-back "\\s_\\|\\sw")
      (paredit-backward))
    (let ((beg (point)))
      (paredit-forward)
      (buffer-substring-no-properties beg (point)))))

;;;###autoload
(defun cljr-destructure-keys ()
  "Change a symbol binding at point to a destructuring bind.
Keys to use in the destructuring are inferred from the code, and
their usage is replaced with the new local variables.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-destructure-keys"
  (interactive)
  (save-excursion
    (paredit-backward-up)
    (unless (looking-at "\\[")
      (error "Place point on the symbol to destructure inside the [let form]")))
  (let* ((symbol (cljr--find-symbol-at-point))
         (re (concat "(:\\(\\sw\\|\\s_\\)+ " (regexp-quote symbol) ")"))
         (bound (save-excursion
                  (paredit-backward-up 2)
                  (paredit-forward)
                  (point)))
         symbols include-as)
    (save-excursion ;; collect all symbols
      (paredit-forward)
      (while (re-search-forward re bound t)
        (paredit-backward)
        (paredit-forward-down)
        (paredit-raise-sexp)
        (delete-char 1)
        (!cons (cljr--find-symbol-at-point) symbols)))
    (save-excursion ;; find new bound
      (paredit-backward-up 2)
      (paredit-forward)
      (setq bound (point)))
    (save-excursion ;; are there any more usages of symbol?
      (paredit-forward-up)
      (when (re-search-forward (regexp-opt (list symbol) 'symbols) bound t)
        (setq include-as t)))
    (when (looking-back "\\s_\\|\\sw")
      (paredit-backward))
    (kill-sexp)
    (insert "{:keys [" (s-join " " (-distinct (reverse symbols))) "]"
            (if include-as (concat " :as " symbol) "") "}")))

;; ------ Cycling ----------

;;;###autoload
(defun cljr-cycle-privacy ()
  "Make public the current private def, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy"
  (interactive)
  (save-excursion
    (ignore-errors (forward-char 7))
    (search-backward-regexp "\\((defn-? \\)\\|\\((def \\)")
    (cond
     ((and cljr-use-metadata-for-privacy
           (looking-at "(defn ^:private"))
      (forward-char 5)
      (delete-char 10))
     ((and (not cljr-use-metadata-for-privacy)
           (looking-at "(defn-"))
      (forward-char 5)
      (delete-char 1))
     ((and cljr-use-metadata-for-privacy
           (looking-at "(defn"))
      (forward-char 5)
      (insert " ^:private"))
     ((and (not cljr-use-metadata-for-privacy)
           (looking-at "(defn"))
      (forward-char 5)
      (insert "-"))
     ((looking-at "(def ^:private")
      (forward-char 5)
      (delete-char 10))
     ((looking-at "(def ")
      (forward-char 5)
      (insert "^:private ")))))

;;;###autoload
(defun cljr-cycle-coll ()
  "Convert the coll at (point) from (x) -> {x} -> [x] -> -> #{x} -> (x) recur

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-coll"
  (interactive)
  (save-excursion
    (while (and
            (> (point) 1)
            (not (eq (string-to-char "(") (char-after)))
            (not (string= "#{" (buffer-substring (point) (+ 2 (point)))))
            (not (eq (string-to-char "{") (char-after)))
            (not (eq (string-to-char "[") (char-after))))
      (backward-char))

    (cond
     ((eq (string-to-char "(") (char-after))
      (insert "{" (substring (cljr--delete-and-extract-sexp) 1 -1) "}"))

     ((eq (string-to-char "#") (char-after))
      (delete-char 1)
      (insert "(" (substring (cljr--delete-and-extract-sexp) 1 -1) ")"))

     ((eq (string-to-char "{") (char-after))
      (if (not (equal (string-to-char "#") (char-before)))
          (insert "[" (substring (cljr--delete-and-extract-sexp) 1 -1) "]")
        (backward-char)
        (delete-char 1)
        (insert "(" (substring (cljr--delete-and-extract-sexp) 1 -1) ")")))

     ((eq (string-to-char "[") (char-after))
      (insert "#{" (substring (cljr--delete-and-extract-sexp) 1 -1) "}"))

     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))))

(defun cljr--goto-if ()
  (while (not (or (cljr--toplevel-p)
                  (looking-at "\\((if \\)\\|\\((if-not \\)")))
    (paredit-backward-up)))

;;;###autoload
(defun cljr-cycle-if ()
  "Change a surrounding if to if-not, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-if"
  (interactive)
  (save-excursion
    (cljr--goto-if)
    (cond
     ((looking-at "(if-not")
      (forward-char 3)
      (delete-char 4)
      (paredit-forward)
      (paredit-forward)
      (transpose-sexps 1))
     ((looking-at "(if")
      (forward-char 3)
      (insert "-not")
      (paredit-forward)
      (paredit-forward)
      (transpose-sexps 1)))))

;;;###autoload
(defun cljr-raise-sexp (&optional argument)
  "Like paredit-raise-sexp, but removes # in front of function literals and sets."
  (interactive "P")
  (paredit-raise-sexp argument)
  (when (looking-back " #")
    (delete-char -1)))

;;;###autoload
(defun cljr-splice-sexp-killing-backward (&optional argument)
  "Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets."
  (interactive "P")
  (paredit-splice-sexp-killing-backward argument)
  (when (looking-back " #")
    (delete-char -1)))

;;;###autoload
(defun cljr-splice-sexp-killing-forward (&optional argument)
  "Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets."
  (interactive "P")
  (save-excursion
    (paredit-backward-up)
    (when (looking-back " #")
      (delete-char -1)))
  (paredit-splice-sexp-killing-forward argument))

;; ------ magic requires -------

(defun cljr--magic-requires-re ()
  (concat "(\\(" (regexp-opt (-map 'car cljr-magic-require-namespaces)) "\\)/"))

;;;###autoload
(defun cljr-slash ()
  "Inserts / as normal, but also checks for common namespace shorthands to require.
If `cljr-magic-require-namespaces' is non-nil, typing one of the
short aliases listed in `cljr-magic-requires' followed by this
command will add the corresponding require statement to the ns
form."
  (interactive)
  (insert "/")
  (when (and cljr-magic-requires
             (looking-back (cljr--magic-requires-re) (point-at-bol)))
    (let* ((short (match-string-no-properties 1))
           (long (aget cljr-magic-require-namespaces short)))
      (if (and (not (cljr--in-namespace-declaration? (concat ":as " short)))
               (or (not (eq :prompt cljr-magic-requires))
                   (yes-or-no-p (format "Add %s :as %s to requires?" long short))))
          (save-excursion
            (cljr--insert-in-ns ":require")
            (insert (format "[%s :as %s]" long short))
            (cljr--maybe-sort-ns))))))

(defun aget (map key)
  (cdr (assoc key map)))

(defun cljr--in-namespace-declaration? (s)
  (save-excursion
    (cljr--goto-ns)
    (cljr--search-forward-within-sexp s)))

;; ------ project clean --------

(defun cljr--excluded-from-project-clean? (filename)
  (member (s-with filename
            (s-chop-prefix (cljr--project-dir))
            (s-chop-prefix "/"))
          cljr-project-clean-exceptions))

;;;###autoload
(defun cljr-project-clean ()
  "Run `cljr-project-clean-functions' on every clojure file, then
sorts the project's dependency vectors.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-project-clean"
  (interactive)
  (when (or (not cljr-project-clean-prompt)
            (yes-or-no-p "Cleaning your project might change many of your clj files. Do you want to proceed?"))
    (dolist (filename (cljr--project-files))
      (when (and (cljr--clojure-filename-p filename)
                 (not (cljr--excluded-from-project-clean? filename)))
        (cljr--update-file filename
          (ignore-errors (-map 'funcall cljr-project-clean-functions)))))
    (when cljr-project-clean-sorts-project-dependencies
      (cljr-sort-project-dependencies))
    (if (and (cider-connected-p) (nrepl-op-supported-p "warm-ast-cache"))
        (cljr--warm-ast-cache))
    (message "Project clean done.")))

(defun cljr--extract-dependency-name ()
  (assert (cljr--looking-at-dependency-vector-p))
  (forward-char)
  (prog1
      (buffer-substring-no-properties
       (point)
       (cljr--point-after '(re-search-forward "\\s-") 'backward-char))
    (backward-char)
    (cljr--delete-and-extract-sexp)
    (delete-region (point-at-bol) (point-at-eol))
    (forward-line)
    (join-line)))

(defun cljr--empty-buffer? (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (s-blank? (s-trim (buffer-substring-no-properties (point-min) (point-max)))))))

(defun cljr--extract-next-dependency-name ()
  (while (not (or (cljr--empty-buffer?)
                  (cljr--looking-at-dependency-vector-p)))
    (delete-char 1))
  (when (cljr--looking-at-dependency-vector-p)
    (cljr--extract-dependency-name)))

(defun cljr--get-sorted-dependency-names (deps)
  "Strips metadata and comments"
  (with-temp-buffer
    (let ((names (list)))
      (insert (->> deps (s-chop-prefix "[") (s-chop-suffix "]")))
      (goto-char (point-min))
      (while (not (cljr--empty-buffer?))
        (push (cljr--extract-next-dependency-name) names))
      (s-join "\n "(-sort #'string< names)))))

(defun cljr--prepare-sort-buffer (dividing-line)
  (insert sorted-names)
  (goto-char (point-max))
  (open-line 1)
  (forward-line)
  (insert dividing-line)
  (open-line 1)
  (forward-line)
  (insert vectors-and-meta))

(defun cljr--sort-dependency-vectors-with-meta-and-comments (dividing-line)
  ;;; The buffer looks like this:
  ;;; foo/bar
  ;;; <dividing-line>
  ;;; ^:src-dep [foo/bar "0.1.1"]

  ;;; Until we cross the dividing line, take a sorted line, find
  ;;; its equal in the raw content below and move that vector with meta and
  ;;; comments to the end of the buffer
  (goto-char (point-min))
  (while (not (looking-at dividing-line))
    (let ((dep (s-trim (cljr--extract-region (point) (point-at-eol))))
          start end vector-and-meta)
      (forward-line)
      (join-line)
      (re-search-forward dividing-line)
      (re-search-forward (s-concat "\\[" dep "\\s-+\""))
      (paredit-backward-up 2)
      (while (not (looking-back "^\\s-*"))
        (forward-char -1))
      (while (save-excursion (forward-line -1) (cljr--comment-line?))
        (forward-line -1))
      (setq start (point))
      (re-search-forward (s-concat "\\[" dep "\\s-+\""))
      (setq end (max (point-at-eol)
                     (cljr--point-after
                      '(paredit-forward-up 2) '(move-end-of-line 1))))
      (setq vector-and-meta (buffer-substring-no-properties start end))
      (delete-region start end)
      (forward-line)
      (join-line)
      (goto-char (point-max))
      (open-line 1)
      (forward-line)
      (insert vector-and-meta)
      (goto-char (point-min))))
  (cljr--delete-line))

(defun cljr--sort-dependency-vectors (sorted-names vectors-and-meta)
  (with-temp-buffer
    (let ((dividing-line "<===============================>"))
      (cljr--prepare-sort-buffer dividing-line)
      (cljr--sort-dependency-vectors-with-meta-and-comments dividing-line)
      (->> (buffer-substring-no-properties (point) (point-max))
           s-trim
           (s-prepend "[")
           (s-append "]")))))

;;;###autoload
(defun cljr-sort-project-dependencies ()
  "Sorts all dependency vectors in project.clj

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-sort-project-dependencies"
  (interactive)
  (cljr--assert-leiningen-project)
  (cljr--update-file (cljr--project-file)
    (goto-char (point-min))
    (while (re-search-forward ":dependencies" (point-max) t)
      (forward-char)
      (-> (buffer-substring-no-properties (point)
                                          (cljr--point-after 'paredit-forward))
          cljr--get-sorted-dependency-names
          (cljr--sort-dependency-vectors (->> (cljr--delete-and-extract-sexp)
                                              (s-chop-prefix "[")
                                              (s-chop-suffix "]")))
          insert))
    (indent-region (point-min) (point-max))
    (save-buffer)))

(defun cljr--call-middleware-sync (request &optional key)
  "Call the middleware with REQUEST.

If it's present KEY indicates the key to extract from the response."
  (let* ((nrepl-sync-request-timeout 25)
         (response (-> request nrepl-send-sync-request cljr--maybe-rethrow-error)))
    (if key
        (nrepl-dict-get response key)
      response)))

(defun cljr--call-middleware-async (request &optional callback)
  (nrepl-send-request request callback))

(defun cljr--get-artifacts-from-middleware (force)
  (message "Retrieving list of available libraries...")
  (let* ((request (list "op" "artifact-list" "force" (if force "true" "false")))
         (artifacts (cljr--call-middleware-sync request "artifacts")))
    (if artifacts
        artifacts
      (error "Empty artifact list received from middleware!"))))

(defun cljr--update-artifact-cache ()
  (cljr--call-middleware-async (list "op" "artifact-list"
                                     "force" "true")
                               (lambda (_)
                                 (when cljr--debug-mode
                                   (message "Artifact cache updated")))))

(defun cljr--dictionary-lessp (str1 str2)
  "return t if STR1 is < STR2 when doing a dictionary compare
(splitting the string at numbers and doing numeric compare with them).
It is optimized for version comparisons, in that empty strings are sorted
before non-empty. This lets 1.7.0 be sorted above 1.7.0-RC1."
  (let ((str1-components (cljr--dict-split str1))
        (str2-components (cljr--dict-split str2)))
    (cljr--dict-lessp str1-components str2-components)))

(defun cljr--dict-lessp (slist1 slist2)
  "compare the two lists of strings & numbers"
  (cond ((null slist1)
         (not (null slist2)))
        ((null slist2)
         t)
        ((and (numberp (car slist1))
              (stringp (car slist2)))
         t)
        ((and (numberp (car slist2))
              (stringp (car slist1)))
         nil)
        ((and (numberp (car slist1))
              (numberp (car slist2)))
         (or (< (car slist1) (car slist2))
             (and (= (car slist1) (car slist2))
                  (cljr--dict-lessp (cdr slist1) (cdr slist2)))))
        (t
         (or (string-lessp (car slist1) (car slist2))
             (and (string-equal (car slist1) (car slist2))
                  (cljr--dict-lessp (cdr slist1) (cdr slist2)))))))

(defun cljr--dict-split (str)
  "split a string into a list of number and non-number components"
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]+" str)))
          (cond ((null p)
                 (setq res (cons str res))
                 (setq str nil))
                ((= p 0)
                 (setq res (cons (string-to-number (match-string 0 str)) res))
                 (setq str (substring str (match-end 0))))
                (t
                 (setq res (cons (substring str 0 (match-beginning 0)) res))
                 (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun cljr--get-versions-from-middleware (artifact)
  (let* ((request (list "op" "artifact-versions" "artifact" artifact))
         (versions (nreverse (sort (cljr--call-middleware-sync request "versions") 'cljr--dictionary-lessp))))
    (if versions
        versions
      (error "Empty version list received from middleware!"))))

(defun cljr--prompt-user-for (prompt &optional choices)
  (completing-read prompt choices))

(defun cljr--add-project-dependency (artifact version)
  (cljr--update-file (cljr--project-file)
    (goto-char (point-min))
    (re-search-forward ":dependencies")
    (paredit-forward)
    (paredit-backward-down)
    (newline-and-indent)
    (insert "[" artifact " \"" version "\"]")
    (message "Added %s version %s as a project dependency" artifact version)
    (when cljr-hotload-dependencies
      (paredit-backward-down)
      (cljr-hotload-dependency))))

(defun cljr--assert-middleware ()
  (unless (featurep 'cider)
    (error "CIDER isn't installed!"))
  (unless (cider-connected-p)
    (error "CIDER isn't connected!"))
  (unless (nrepl-op-supported-p "find-symbol")
    (error "nrepl-refactor middleware not available! Did you remember to install it?")))

(defun cljr--ensure-op-supported (op)
  "Check for support of middleware op OP.
Signal an error if it is not supported."
  (cljr--assert-middleware)
  (unless (nrepl-op-supported-p op)
    (error "Can't find nREPL middleware providing op \"%s\".  Please, install (or update) refactor-nrepl %s and restart the REPL." op (upcase cljr-version))))

(defun cljr--assert-leiningen-project ()
  (unless (string= (file-name-nondirectory (or (cljr--project-file) ""))
                   "project.clj")
    (error "Can't find project.clj!")))

;;;###autoload
(defun cljr-add-project-dependency (force)
  "Add a dependency to the project.clj file.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-project-dependency"
  (interactive "P")
  (cljr--assert-leiningen-project)
  (cljr--ensure-op-supported "artifact-list")
  (-when-let* ((lib-name (->> (cljr--get-artifacts-from-middleware force)
                              (cljr--prompt-user-for "Artifact: ")))
               (version (->> (cljr--get-versions-from-middleware lib-name)
                             (cljr--prompt-user-for "Version: "))))
    (cljr--add-project-dependency lib-name version)))

;;;###autoload
(defun cljr-update-project-dependency ()
  "Update the version of the dependency at point."
  (interactive)
  (cljr--assert-leiningen-project)
  (cljr--ensure-op-supported "artifact-list")
  (unless (cljr--looking-at-dependency-vector-p)
    (error "Place cursor in front of dependency vector to update."))
  (save-excursion
    (let (lib-name current-version)
      (paredit-forward-down)
      (setq lib-name (cljr--extract-sexp))
      (paredit-forward)
      (skip-syntax-forward " ")
      (setq current-version (cljr--extract-sexp))
      (let ((version (->> (cljr--get-versions-from-middleware lib-name)
                          (cljr--prompt-user-for (concat lib-name " version: ")))))
        (cljr--delete-sexp)
        (insert "\"" version "\""))))
  (when cljr-hotload-dependencies
    (cljr-hotload-dependency)
    (cljr--ensure-op-supported "artifact-list")))

;;;###autoload
(defun cljr-update-project-dependencies ()
  "Update all project dependencies.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-update-project-dependencies"
  (interactive)
  (cljr--assert-leiningen-project)
  (find-file (cljr--project-file))
  (goto-char (point-min))
  (let (cljr-hotload-dependencies)
    (while (re-search-forward ":dependencies" (point-max) t)
      (paredit-forward-down)
      (cljr--skip-past-whitespace-and-comments)
      (while (not (looking-at "]"))
        (let ((highlight (cljr--highlight-sexp)))
          (unwind-protect
              (cljr-update-project-dependency)
            (delete-overlay highlight)))
        (paredit-forward)
        (cljr--skip-past-whitespace-and-comments)))))

(defun cljr--skip-past-whitespace-and-comments ()
  (skip-syntax-forward " >")
  (while (looking-at ";")
    (move-end-of-line 1)
    (forward-char)
    (skip-syntax-forward " >")))

(defun cljr--extract-anon-fn-name (sexp-str)
  (when (string-match "(fn \\(\\_<[^ ]+\\_>\\)?" sexp-str)
    (match-string-no-properties 1 sexp-str)))

(defun cljr--highlight (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'priority 100)
    overlay))

(defun cljr--highlight-sexp ()
  (cljr--highlight (point) (cljr--point-after 'paredit-forward)))

(defun cljr--promote-fn ()
  (save-excursion
    (let* ((locals (save-excursion (paredit-forward-down)
                                   (cljr--call-middleware-to-find-unbound-vars
                                    (buffer-file-name) (line-number-at-pos)
                                    (1+ (current-column)))))
           (fn (cljr--extract-sexp))
           (namedp (cljr--extract-anon-fn-name fn))
           (name (or namedp
                     (unless (cljr--use-multiple-cursors?)
                       (let ((highlight (cljr--highlight-sexp)))
                         (unwind-protect
                             (read-string "Name: ")
                           (delete-overlay highlight))))))
           fn-start)
      (cljr--delete-sexp)
      (save-excursion
        (cljr--new-toplevel-form fn)
        (paredit-backward-down)
        (cljr--goto-fn-definition)
        (setq fn-start (point))
        (forward-char)
        (insert "de")
        (paredit-forward)
        (when cljr-favor-private-functions
          (if cljr-use-metadata-for-privacy
              (insert " ^:private")
            (insert "-")))
        (when (not namedp)
          (insert " ")
          (newline)
          (backward-char)
          (if name
              (insert name)
            (mc/create-fake-cursor-at-point)))
        (re-search-forward "\\[")
        (when (s-present? locals)
          (insert locals)
          (unless (looking-at-p "\\]")
            (insert " ")))
        (paredit-forward-up)
        (unless (looking-at "\s*?$")
          (newline))
        (indent-region fn-start (cljr--point-after 'paredit-forward-up)))
      (when (s-present? locals)
        (insert (format "(partial  %s)" locals))
        (backward-char (length (concat " " locals ")"))))
      (if name
          (insert name)
        (mc/maybe-multiple-cursors-mode)))))

(defun cljr--append-fn-parameter (param)
  (cljr--goto-fn-definition)
  (paredit-forward-down)
  (paredit-forward 2)
  (paredit-backward-down)
  (if (looking-back "\\[")
      (insert param)
    (insert " " param)))

(defun cljr--promote-function-literal ()
  (delete-char 1)
  (let ((body (cljr--delete-and-extract-sexp)))
    (insert "(fn [] " body ")"))
  (backward-char)
  (cljr--goto-fn-definition)
  (let ((fn-start (point))
        var replacement)
    (while (re-search-forward "%[1-9]?" (cljr--point-after 'paredit-forward) t)
      (setq var (buffer-substring (point) (cljr--point-after 'paredit-backward)))
      (setq replacement (read-string (format "%s => " var)))
      (cljr--append-fn-parameter replacement)
      (replace-regexp (format "\\s-%s\\(\\s-\\|\\|\n)\\)" var)
                      (format " %s\\1" replacement)
                      nil
                      fn-start
                      (save-excursion (paredit-forward-up 2) (point)))
      (goto-char fn-start))))

;;;###autoload
(defun cljr-promote-function (promote-to-defn)
  "Promote a function literal to an fn, or an fn to a defn.
With prefix PROMOTE-TO-DEFN, promote to a defn even if it is a
function literal.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-promote-function"
  (interactive "P")
  (save-excursion
    (cond
     ;; Already in the right place.
     ((or (looking-at-p "#(")
          (looking-at-p "(fn")))
     ;; Right after the #.
     ((and (eq (char-after) ?\()
           (eq (char-before) ?#))
      (forward-char -1))
     ;; Possibly inside a function.
     (t (cljr--goto-fn-definition)))
    ;; Now promote it.
    (if (looking-at "#(")
        (cljr--promote-function-literal)
      (cljr--promote-fn)))
  (when current-prefix-arg
    (cljr--promote-fn)))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-promote-function)

(defun cljr--insert-in-find-symbol-buffer (occurrence)
  (save-excursion
    (pop-to-buffer cljr--find-symbol-buffer)
    (end-of-buffer)
    (insert occurrence)))

(defun cljr--end-of-buffer? ()
  "True if point is at end of buffer"
  (= (point) (cljr--point-after 'end-of-buffer)))

(defun cljr--find-symbol-sync (symbol ns)
  (let* ((filename (buffer-file-name))
         (line (line-number-at-pos))
         (column (1+ (current-column)))
         (dir (cljr--project-dir))
         (request (list "op" "find-symbol"
                        "ns" ns
                        "dir" dir
                        "file" filename
                        "line" line
                        "column" column
                        "name" symbol))
         occurrences)
    (with-temp-buffer
      (insert (cljr--call-middleware-sync request "occurrence"))
      (unless (cljr--empty-buffer?)
        (goto-char (point-min))
        (while (not (cljr--end-of-buffer?))
          (push (read (current-buffer)) occurrences))))
    occurrences))

(defun cljr--find-symbol (symbol ns callback)
  (let* ((filename (buffer-file-name))
         (line (line-number-at-pos))
         (column (1+ (current-column)))
         (dir (cljr--project-dir))
         (find-symbol-request (list "op" "find-symbol"
                                    "ns" ns
                                    "dir" dir
                                    "file" filename
                                    "line" line
                                    "column" column
                                    "name" symbol
                                    "ignore-errors" (when cljr-find-usages-ignore-analyzer-errors "true"))))
    (with-current-buffer (nrepl-current-connection-buffer)
      (setq cjr--occurrence-count 0)
      (setq cljr--num-syms -1))
    (cljr--call-middleware-async find-symbol-request callback)))

(defun cljr--first-line (s)
  (-> s s-lines car s-trim))

(defun cljr--project-relative-path (path)
  "Denormalize PATH to make to make it relative to the project
root."
  (s-chop-prefix (cljr--project-dir) path))

(defun cljr--format-symbol-occurrence (occurrence)
  (let ((file (plist-get occurrence :file))
        (line (plist-get occurrence :line-beg))
        (match (plist-get occurrence :match )))
    (format "%s:%s: %s\n" (cljr--project-relative-path file) line
            (cljr--first-line match))))

(defun cljr--format-and-insert-symbol-occurrence (occurrence-resp)
  (let ((occurrence (nrepl-dict-get occurrence-resp "occurrence"))
        (count (nrepl-dict-get occurrence-resp "count"))
        (cljr--find-symbol-buffer "*cljr-find-usages*"))
    (cljr--maybe-rethrow-error occurrence-resp)
    (when count
      (setq cljr--num-syms count))
    (when occurrence
      (incf cjr--occurrence-count)
      (->> occurrence
           read
           cljr--format-symbol-occurrence
           cljr--insert-in-find-symbol-buffer))
    (when (= cjr--occurrence-count cljr--num-syms)
      (cljr--finalise-find-symbol-buffer cljr--num-syms))))

(defun cljr--finalise-find-symbol-buffer (num-of-symbols)
  (with-current-buffer "*cljr-find-usages*"
    (insert (format "\nFind symbol finished: %d occurrence%s found"
                    num-of-symbols (if (> num-of-symbols 1) "s" "")))
    ;; Place point on first occurrence
    (goto-char (point-min))
    (forward-line 2)))

(defun cljr--setup-find-symbol-buffer (symbol-name)
  (save-window-excursion
    (when (get-buffer cljr--find-symbol-buffer)
      (kill-buffer cljr--find-symbol-buffer))
    (pop-to-buffer cljr--find-symbol-buffer)
    (with-current-buffer "*cljr-find-usages*"
      (insert (format "'%s' occurs in the following places:\n\n" symbol-name))
      (grep-mode)
      (setq buffer-read-only nil)
      (setq-local compilation-search-path (list (cljr--project-dir))))))

;;;###autoload
(defun cljr-find-usages ()
  "Find all usages of the symbol at point in the project.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-find-usages"
  (interactive)
  (cljr--assert-middleware)
  (save-buffer)
  (let* ((cljr--find-symbol-buffer "*cljr-find-usages*")
         (symbol (cider-symbol-at-point))
         (var-info (cider-var-info symbol))
         (ns (nrepl-dict-get var-info "ns"))
         (symbol-name (nrepl-dict-get var-info "name")))
    (cljr--setup-find-symbol-buffer (or symbol-name symbol))
    (cljr--find-symbol (or symbol-name symbol) ns 'cljr--format-and-insert-symbol-occurrence)))

(defun cljr--rename-occurrence (file line-beg col-beg name new-name)
  (save-excursion
    (with-current-buffer
        (find-file-noselect file)
      (let* ((name (->> name cljr--symbol-suffix regexp-quote)))
        (goto-char (point-min))
        (forward-line (1- line-beg))
        (move-to-column (1- col-beg))
        ;; When the match is a definition, the position of the symbol
        ;; isn't returned but the beginning of the defining form
        (when (looking-at-p "(\\s-*def")
          (re-search-forward name)
          (paredit-backward))
        (when (looking-at (format "\\(.+/\\)?\\(%s\\)" name))
          (replace-match (format "\\1%s" new-name))))
      (save-buffer))))

(defun cljr--rename-occurrences (ns occurrences new-name)
  (dolist (symbol-meta occurrences)
    (let* ((file (plist-get symbol-meta :file))
           (line-beg (plist-get symbol-meta :line-beg))
           (col-beg (plist-get symbol-meta :col-beg))
           (name (plist-get symbol-meta :name)))
      (cljr--rename-occurrence file line-beg col-beg name new-name))))

;;;###autoload
(defun cljr-rename-symbol ()
  "Rename the symbol at point and all of its occurrences.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-rename-symbol"
  (interactive)
  (cljr--assert-middleware)
  (save-buffer)
  (let* ((symbol (cider-symbol-at-point))
         (var-info (cider-var-info symbol))
         (symbol-name (nrepl-dict-get var-info "name"))
         (ns (nrepl-dict-get var-info "ns"))
         (name (or symbol-name symbol))
         (_ (message "Fetching symbol occurrences..."))
         (occurrences (cljr--find-symbol-sync name ns))
         (new-name (read-from-minibuffer "New name: " (cljr--symbol-suffix symbol)))
         (buffer-of-symbol (cider-find-var-file (concat ns "/" symbol-name)))
         (tooling-buffer-p (cider--tooling-file-p (buffer-name buffer-of-symbol))))
    (cljr--rename-occurrences ns occurrences new-name)
    (message "Renamed %s occurrences of %s" (length occurrences) name)
    (when (> (length occurrences) 0)
      (cljr--warm-ast-cache))))

(defun cljr--maybe-nses-in-bad-state (response)
  (let ((asts-in-bad-state (->> (nrepl-dict-get response "ast-statuses")
                                edn-read
                                (-partition 2)
                                (--filter (not (stringp (-last-item it)))))))
    (when (not (= 0 (length asts-in-bad-state)))
      (user-error (concat "Some namespaces are in a bad state: "
                          (->> asts-in-bad-state
                               (--map (format "error \"%s\" in %s" (-last-item (-last-item it)) (-first-item it)))
                               (s-join "; ")))))))

(defun cljr--warm-ast-cache ()
  (cljr--call-middleware-async
   (list "op" "warm-ast-cache")
   (lambda (res)
     (cljr--maybe-rethrow-error res)
     (cljr--maybe-nses-in-bad-state res)
     (when cljr--debug-mode
       (message "AST index updated")))))

(defun cljr--replace-ns (new-ns)
  (save-excursion
    (cljr--goto-ns)
    (cljr--delete-and-extract-sexp)
    (insert new-ns)
    (cljr--goto-ns)
    (indent-region (point) (cljr--point-after 'paredit-forward))
    (paredit-forward)
    (cljr--just-one-blank-line)))

;;;###autoload
(defun cljr-clean-ns ()
  "Clean the ns form for the current buffer.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-clean-ns"
  (interactive)
  (cljr--assert-leiningen-project)
  (cljr--assert-middleware)
  ;; don't save the buffer preliminarily if we are called from project clean
  (when (not (boundp 'filename))
    (save-buffer))
  ;; if filename *is* bound `cljr-clean-ns was called from project-clean
  ;; so we can *not* use the buffer-file-name as the file was opened in a
  ;; temporary buffer with no file information attached to it
  (let* ((path-to-file (if (boundp 'filename)
                           filename
                         (buffer-file-name)))
         (result (nrepl-send-sync-request
                  (list "op" "clean-ns"
                        "path" path-to-file))))
    (-when-let (error-msg (nrepl-dict-get result "error"))
      (error error-msg))
    (-when-let (new-ns (nrepl-dict-get result "ns"))
      (cljr--replace-ns new-ns))))

(defun cljr--narrow-candidates (candidates)
  (cond ((= (length candidates) 0)
         (error "Couldn't find any symbols matching %s on classpath."
                (cljr--symbol-suffix symbol)))
        ((= (length candidates) 1)
         (car candidates))
        (t
         (cljr--prompt-user-for "Require: " candidates))))

(defun cljr--insert-libspec-verbosely (libspec)
  (insert libspec)
  (message "%s added to ns" libspec))

(defun cljr--insert-missing-import (missing)
  (save-excursion
    (cljr--insert-in-ns ":import")
    (cljr--insert-libspec-verbosely missing)))

(defun cljr--symbol-prefix (symbol)
  "java.util.Date => java.util
str/split => str
split => ''"
  (cond ((s-contains? "/" symbol) (car (s-split "/" symbol)))
        ((s-matches? "\\w+\\.\\w+" symbol)
         (s-join "." (butlast (s-split "\\." symbol))))
        (t "")))

(defun cljr--insert-missing-require (symbol missing)
  (save-excursion
    (cljr--insert-in-ns ":require")
    (let ((alias? (s-contains? "/" symbol)))
      (if alias?
          (cljr--insert-libspec-verbosely (format "[%s :as %s]" missing
                                                  (cljr--symbol-prefix symbol)))
        (if (and (s-contains? "." missing)
                 (s-uppercase? (s-left 1 (cljr--symbol-suffix missing))))
            (cljr--insert-libspec-verbosely  (cljr--symbol-prefix symbol))
          (cljr--insert-libspec-verbosely (format "[%s :refer [%s]]"
                                                  missing symbol)))))))

(defun cljr--add-missing-libspec (symbol candidates-and-types)
  (let* ((candidates (mapcar (lambda (pair) (symbol-name (car pair)))
                             candidates-and-types))
         (missing (cljr--narrow-candidates candidates))
         (type (cadr (assoc (intern missing) candidates-and-types))))
    (cond ((eq type :ns) (cljr--insert-missing-require symbol missing))
          ((eq type :type)
           ;; We need to both require the ns, to trigger compilation,
           ;; and then import the java class

           ;; In the line below we're assuming that all clojure code
           ;; will prefer - over _ when naming namespaces :(
           (progn (cljr--insert-missing-require (s-replace "_" "-" missing)
                                                missing)
                  (cljr--insert-missing-import missing)))
          ((eq type :class) (cljr--insert-missing-import missing))
          (t (error (format "Uknown type %s" type))))))

(defun cljr--symbol-suffix (symbol)
  "java.util.Date => Date
Date => Date
clojure.string/split => split
str/split => split"
  (let ((name (cljr--normalize-symbol-name symbol)))
    (cond
     ((s-matches? "\\w+\\.\\w+" name)
      (->> name (s-split "\\.") last car cljr--symbol-suffix))
     ((s-contains? "/" name)
      (->> name (s-split "/") cadr cljr--symbol-suffix))
     (t name))))

(defun cljr--normalize-symbol-name (name)
  "Removes reader macros and quoting

Date. -> Date
@sym => sym
#'sym => sym
'sym => sym
~sym => sym
~@sym => sym"
  (cond
   ((s-ends-with? "." name)
    (->> name (s-chop-suffix ".") cljr--normalize-symbol-name))
   ((s-starts-with? "#'" name)
    (-> name (s-chop-prefix "#'") cljr--normalize-symbol-name))
   ((s-starts-with? "'" name)
    (->> name (s-chop-prefix "'") cljr--normalize-symbol-name))
   ((s-starts-with? "~" name)
    (->> name (s-chop-prefix "~") cljr--normalize-symbol-name))
   ((s-starts-with? "~@" name)
    (->> name (s-chop-prefix "~@") cljr--normalize-symbol-name))
   ((s-starts-with? "@" name)
    (->> name (s-chop-prefix "@") cljr--normalize-symbol-name))
   (t name)))

(defun cljr--call-middleware-to-resolve-missing (symbol)
  ;; Just so this part can be mocked out in a step definition
  (nrepl-send-sync-request
   (list "op" "resolve-missing"
         "symbol" (cljr--symbol-suffix symbol))))

(defun cljr--get-error-value (response)
  "Gets the error value from the middleware response.

We can't simply call `nrepl-dict-get' because the error value
itself might be `nil'."
  (assert (nrepl-dict-p response) nil
          "Response from middleware isn't an nrepl-dict!")
  (-if-let (err (nrepl-dict-get response "err"))
      (error (format "Error in nrepl-refactor: %s" err))
    (let* ((maybe-error-and-rest
            (-drop-while (lambda (e)
                           (not (and (stringp e) (s-equals? e "error"))))
                         response))
           (maybe-error (car maybe-error-and-rest)))
      (when (and (stringp maybe-error) (s-equals? maybe-error "error"))
        (or (cadr maybe-error-and-rest)
            (format "Error 'nil' returned from middleware. %s"
                    "Please contact your local administrator."))))))

(defun cljr--maybe-rethrow-error (response)
  (-if-let (err (cljr--get-error-value response))
      (error err)
    response))

(defun cljr--maybe-eval-ns-form ()
  (when cljr-auto-eval-ns-form
    (cider-eval-ns-form :synchronously)))

;;;###autoload
(defun cljr-add-missing-libspec ()
  "Requires or imports the symbol at point.

If the symbol at point is of the form str/join then the ns
containing join will be aliased to str.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-missing-libspec"
  (interactive)
  (cljr--assert-middleware)
  (let* ((symbol (cider-symbol-at-point))
         (response (cljr--call-middleware-to-resolve-missing symbol))
         (candidates-and-types (nrepl-dict-get response "candidates")))
    (cljr--maybe-rethrow-error response)
    (if candidates-and-types
        (cljr--add-missing-libspec symbol (read candidates-and-types))
      (error "Can't find %s on classpath" (cljr--symbol-suffix symbol))))
  (cljr--maybe-clean-or-sort-ns)
  (cljr--maybe-eval-ns-form))

(defun cljr--dependency-vector-at-point ()
  (save-excursion
    (ignore-errors
      (while (not (cljr--looking-at-dependency-vector-p))
        (paredit-backward-up))
      (buffer-substring-no-properties (point)
                                      (cljr--point-after 'paredit-forward)))))

(defun cljr--hotload-dependency-callback (response)
  (cljr--maybe-rethrow-error response)
  (message "Hotloaded %s" (nrepl-dict-get response "dependency")))

(defun cljr--call-middleware-to-hotload-dependency (dep)
  (nrepl-send-request
   (list "op" "hotload-dependency"
         "coordinates" dep)
   #'cljr--hotload-dependency-callback))

(defun cljr--assert-dependency-vector (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (assert (cljr--looking-at-dependency-vector-p) nil
            (format
             (s-concat "Expected dependency vector of type "
                       "[org.clojure \"1.7.0\"], but got '%s'")
             string)))
  string)

;;;###autoload
(defun cljr-hotload-dependency ()
  "Download a dependency (if needed) and hotload it into the current repl session.

Defaults to the dependency vector at point, but prompts if none is found.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-hotload-dependency"
  (interactive)
  (cljr--assert-middleware)
  (-> (or (cljr--dependency-vector-at-point)
          (cljr--prompt-user-for "Dependency vector: "))
      cljr--assert-dependency-vector
      cljr--call-middleware-to-hotload-dependency))

(defun cljr--defn-str ()
  (s-concat "(defn"
            (if cljr-favor-private-functions
                (if cljr-use-metadata-for-privacy
                    " ^:private "
                  "- ")
              " ")))

(defun cljr--call-middleware-to-find-unbound-vars (file line column)
  (s-join " "
          (-> (list "op" "find-unbound" "file" file "line" line "column" column)
              nrepl-send-sync-request
              cljr--maybe-rethrow-error
              (nrepl-dict-get "unbound"))))

(defun cljr--goto-enclosing-sexp ()
  (let ((sexp-regexp (rx (or "(" "#{" "{" "["))))
    (unless (looking-at sexp-regexp)
      (paredit-backward-up))
    (when (looking-back "#")
      (forward-char -1))))

;;;###autoload
(defun cljr-extract-function ()
  "Extract the form at (or above) point as a top-level defn.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-extract-function"
  (interactive)
  (cljr--assert-middleware)
  (save-buffer)
  (cljr--goto-enclosing-sexp)
  (let* ((unbound (cljr--call-middleware-to-find-unbound-vars
                   (buffer-file-name) (line-number-at-pos)
                   ;; +1 because the middleware expects indexing from 1
                   ;; +1 more because point has to be inside the sexp,
                   ;; not on the opening paren
                   (+ (current-column) 2)))
         (name (unless (cljr--use-multiple-cursors?)
                 (let ((highlight (cljr--highlight-sexp)))
                   (unwind-protect
                       (cljr--prompt-user-for "Name: ")
                     (delete-overlay highlight)))))
         (body (cljr--delete-and-extract-sexp)))
    (save-excursion
      (cljr--make-room-for-toplevel-form)
      (insert (cljr--defn-str))
      (if name
          (insert name)
        (mc/create-fake-cursor-at-point))
      (newline)
      (indent-according-to-mode)
      (insert "[" unbound "]")
      (newline-and-indent)
      (insert body ")"))
    (insert "(")
    (when name (insert name))
    (save-excursion
      (insert " " unbound ")"))
    (unless name
      (mc/maybe-multiple-cursors-mode))))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-extract-function)

(defun cljr--at-end-of-symbol-at-point ()
  (looking-back (regexp-quote (cider-symbol-at-point)) (point-at-bol)))

(defun cljr--insert-function-stubs (functions)
  (unless (cljr--at-end-of-symbol-at-point)
    (paredit-forward))
  (save-excursion
    (dolist (fn functions)
      (newline-and-indent)
      (insert "(" (gethash :name fn) " " (gethash :parameter-list fn) ")")))
  (when (> (length functions) 0)
    ;; Move cursor to point where the first functino body goes
    (paredit-forward-down)
    (paredit-forward 2)))

;;;###autoload
(defun cljr-add-stubs ()
  "Adds implementation stubs for the interface or protocol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-add-stubs"
  (interactive)
  (cljr--ensure-op-supported "stubs-for-interface")
  (let* ((interface (cider-symbol-at-point))
         (prefix? (cljr--symbol-prefix interface))
         (alias? (cljr--resolve-alias prefix?))
         (interface (if (not (s-blank? prefix?))
                        (if alias?
                            (format "%s/%s" alias? (cljr--symbol-suffix interface))
                          interface)
                      (format "%s/%s" (cider-current-ns) interface)))
         (functions (edn-read (cljr--call-middleware-sync
                               (list "op" "stubs-for-interface"
                                     "interface" interface)
                               "functions"))))
    (cljr--insert-function-stubs functions)))

(defun cljr--delete-definition (definition)
  "Delete a definition as part of inlining a symbol."
  (let ((file (gethash :file definition))
        (line-beg (gethash :line-beg definition))
        (col-beg (gethash :col-beg definition)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (forward-line (1- line-beg))
      (forward-char (1- col-beg))
      (cljr--delete-and-extract-sexp)
      (when (cljr--inside-let-binding-form-p)
        (cljr--delete-and-extract-sexp)
        (if (save-excursion (cljr--get-let-bindings))
            (progn
              (while (looking-at-p "\s*\n")
                (forward-line)
                (join-line))
              (when (looking-at-p "]")
                ;; we just deleted the last binding in the vector
                (join-line)))
          (cljr--eliminate-let))
        (cljr--indent-defun))
      (when (looking-at-p "\s*\n")
        (cljr--just-one-blank-line))
      (save-buffer))))

(defun cljr--sort-occurrences (occurrences)
  "Sort the occurrences so the last ones in the file comes first."
  (-sort (lambda (o1 o2)
           (let ((o1-line (gethash :line-beg o1))
                 (o2-line (gethash :line-beg o2))
                 (o1-col (gethash :col-beg o1))
                 (o2-col (gethash :col-beg o2)))
             (cond
              ((< o1-line o2-line) o2)
              ((> o1-line o2-line) o1)
              ((< o1-col o2-col ) o2)
              ((> o1-col o2-col) o1)
              (t (error "Sort occurrences failed to compare %s %s %s %s"
                        o1-line o2-line o1-col o2-col)))))
         occurrences))

(defun cljr--inline-fn-at-call-site (def call-site)
  "Point is at a call site, where the sexp call-site has just
  been extracted."
  (let ((args (rest call-site))
        (params (with-temp-buffer
                  (insert def)
                  (goto-char (point-min))
                  (paredit-forward-down 2)
                  (cljr--extract-sexp-as-list)))
        (def (with-temp-buffer
               (insert def)
               (goto-char (point-min))
               (paredit-forward-down 2)
               (paredit-forward-up)
               (paredit-splice-sexp-killing-backward)
               (buffer-string))))
    (dotimes (i (length args))
      (setq def (replace-regexp-in-string (format "\\_<%s\\_>" (nth i params))
                                          (nth i args) def t t)))
    (insert def)))

(defun cljr--inline-symbol (ns definition occurrences)
  (dolist (symbol-meta (cljr--sort-occurrences occurrences))
    (let* ((file (gethash :file symbol-meta))
           (line-beg (gethash :line-beg symbol-meta))
           (col-beg (gethash :col-beg symbol-meta))
           (def (gethash :definition definition))
           (fn? (s-matches-p "^.+fn" def)))
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (forward-line (1- line-beg))
        (forward-char (1- col-beg))
        (let* ((call-site? (looking-back "(\s*"))
               (sexp (if call-site?
                         (prog1 (cljr--extract-sexp-as-list)
                           (paredit-backward-up)
                           (cljr--delete-and-extract-sexp))
                       (cljr--delete-and-extract-sexp))))
          (if call-site?
              (cljr--inline-fn-at-call-site def sexp)
            (insert def))))))
  (save-buffer)
  (cljr--delete-definition definition))

;;;###autoload
(defun cljr-inline-symbol ()
  "Inline the symbol at point.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-inline-symbol"
  (interactive)
  (cljr--ensure-op-supported "extract-definition")
  (save-buffer)
  (save-excursion
    (let* ((filename (buffer-file-name))
           (line (line-number-at-pos))
           (column (1+ (current-column)))
           (dir (cljr--project-dir))
           (symbol (cider-symbol-at-point))
           (var-info (cider-var-info symbol))
           (ns (or (nrepl-dict-get var-info "ns") (cider-current-ns)))
           (symbol-name (or (nrepl-dict-get var-info "name") symbol))
           (extract-definition-request (list
                                        "op" "extract-definition"
                                        "ns" ns
                                        "dir" dir
                                        "file" filename
                                        "line" line
                                        "column" column
                                        "name" symbol-name))
           (response (edn-read (cljr--call-middleware-sync
                                extract-definition-request "definition")))
           (definition (gethash :definition response))
           (occurrences (gethash :occurrences response)))
      (cljr--inline-symbol ns definition occurrences)
      (unless *cljr--noninteractive* ; don't spam when called from `cljr-remove-let'
        (if occurrences
            (message "Inlined %s occurrence(s) of '%s'" (length occurrences) symbol)
          (message "No occurrences of '%s' found.  Deleted the definition." symbol)))))
  (cljr--indent-defun))

(defun cljr--configure-middleware (&optional callback)
  (when (nrepl-op-supported-p "configure")
    (let ((opts (concat "{:prefix-rewriting "
                        (if cljr-favor-prefix-notation "true" "false")
                        " :debug " (if cljr--debug-mode "true" "false")
                        "}")))
      (-> (list "op" "configure" "opts" opts)
          (nrepl-send-request (or callback (lambda (_))))))))

;;;###autoload
(defun cljr-reload-config ()
  "Resend configuration settings to the middleware.

This can be used to avoid restarting the repl session after
changing settings."
  (interactive)
  (cljr--assert-middleware)
  (cljr--configure-middleware
   (lambda (response)
     (cljr--maybe-rethrow-error response)
     (message "Config successfully updated!"))))

(defun cljr--check-nrepl-ops ()
  "Check whether all nREPL ops are present and emit a warning when not."
  (let ((missing-ops (-remove 'nrepl-op-supported-p cljr--nrepl-ops)))
    (when missing-ops
      (cider-repl-emit-interactive-err-output
       (format "WARNING: The following nREPL ops are not supported:
%s\nPlease, install (or update) refactor-nrepl and restart REPL.
You can mute this warning by changing cljr-suppress-middleware-warnings."
               (s-join " " missing-ops ))))))

(defun cljr--check-middleware-version ()
  "Check whether clj-refactor and nrepl-refactor versions are the same"
  (let ((refactor-nrepl-version (or (cljr--middleware-version) "n/a")))
    (unless (s-equals? (s-downcase refactor-nrepl-version)
                       (s-downcase cljr-version))
      (cider-repl-emit-interactive-err-output
       (format "WARNING: clj-refactor and refactor-nrepl are out of sync.
Their versions are %s and %s, respectively.
You can mute this warning by changing cljr-suppress-middleware-warnings."
               cljr-version refactor-nrepl-version)))))

(defun cljr--middleware-version ()
  (cljr--assert-middleware)
  (cljr--call-middleware-sync (list "op" "version") "version"))

;;;###autoload
(defun cljr-version ()
  "Returns the version of the middleware as well as this package."
  (interactive)
  (message "clj-refactor %s, refactor-nrepl %s"
           cljr-version (cljr--middleware-version)))

;;;###autoload
(defun cljr-toggle-debug-mode ()
  (interactive)
  (cljr--ensure-op-supported "configure")
  (cljr--configure-middleware
   (lambda (response)
     (cljr--maybe-rethrow-error response)
     (setq cljr--debug-mode (not cljr--debug-mode))
     (if cljr--debug-mode
         (message "Debug mode on")
       (message "Debug mode off")))))

(defun cljr--init-middleware ()
  (unless cljr-suppress-middleware-warnings
    (cljr--check-nrepl-ops)
    (cljr--check-middleware-version))
  (cljr--configure-middleware)
  ;; Best effort; don't freak people out with errors
  (ignore-errors
    (when cljr-populate-artifact-cache-on-startup
      (cljr--update-artifact-cache))
    (when cljr-eagerly-build-asts-on-startup
      (cljr--warm-ast-cache))))

;;;###autoload
(defun cljr-create-fn-from-example ()
  "Create a top-level defn for the symbol at point.
The context in which symbol is being used should be that of a
function, and the arglist of the defn is guessed from this
context.

For instance, if the symbol is the first argument of a `map'
call, the defn is created with one argument. If it is the first
argument of a `reduce', the defn will take two arguments.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-create-fn-from-example"
  (interactive)
  (while (cljr--is-keyword? (car (cljr--extract-sexp-as-list)))
    (paredit-backward-up))
  (let* ((sexp-forms* (cljr--extract-sexp-as-list))
         (example-name (car sexp-forms*))
         (symbol-at-point (cljr--find-symbol-at-point))
         (parent-fn (ignore-errors
                      (save-excursion
                        (paredit-backward-up 2)
                        (forward-char)
                        (cljr--extract-sexp))))
         (sexp-forms (cond
                      ((or (string= parent-fn "->")
                           (string= parent-fn "->>"))
                       (save-excursion
                         (paredit-backward-up)
                         (cljr--unwind-and-extract-this-as-list example-name)))

                      ((or (string= example-name "->")
                           (string= example-name "->>"))
                       (save-excursion
                         (setq example-name symbol-at-point)
                         (cljr--unwind-and-extract-this-as-list example-name)))

                      (:else sexp-forms*))))
    (push-mark)
    (if (cljr--is-symbol? symbol-at-point)
        (cond ((string= example-name "update")
               (cljr--create-fn-from-update sexp-forms))

              ((string= example-name "update-in")
               (cljr--create-fn-from-update-in))

              ((string= example-name "sort-by")
               (cljr--create-fn-from-sort-by sexp-forms))

              ((string= example-name "sort")
               (cljr--create-fn-from-sort sexp-forms))

              ((string= example-name "reduce")
               (cljr--create-fn-from-reduce sexp-forms))

              ((string= example-name "repeatedly")
               (cljr--insert-example-fn symbol-at-point nil))

              ((member example-name cljr--list-fold-function-names)
               (cljr--create-fn-from-list-fold sexp-forms))

              ((member example-name cljr--list-fold-function-names-with-index)
               (cljr--create-fn-from-list-fold-with-index sexp-forms))

              ((and (featurep 'cider) (cider-connected-p)
                    (cider-var-info example-name :all))
               (cljr--insert-example-fn symbol-at-point (list "args")))

              (:else
               (cljr--insert-example-fn example-name (cdr sexp-forms))))
      (cljr--insert-example-fn example-name (cdr sexp-forms)))))

(defvar cljr--list-fold-function-names
  '("map" "mapv" "pmap" "keep" "mapcat" "filter" "remove" "take-while" "drop-while"
    "group-by" "partition-by" "some" "every?" "not-every?" "not-any?"))

(defvar cljr--list-fold-function-names-with-index
  '("map-indexed" "keep-indexed"))

(defun cljr--create-fn-from-list-fold (sexp-forms)
  (cljr--insert-example-fn (cadr sexp-forms)
                           (--map
                            (-when-let (name (cljr--guess-param-name it))
                              (singularize-string name))
                            (cddr sexp-forms))))

(defun cljr--create-fn-from-list-fold-with-index (sexp-forms)
  (cljr--insert-example-fn (cadr sexp-forms)
                           (cons "index"
                                 (--map
                                  (-when-let (name (cljr--guess-param-name it))
                                    (singularize-string name))
                                  (cddr sexp-forms)))))

(defun cljr--create-fn-from-update (sexp-forms)
  (let ((keyfn (nth 2 sexp-forms)))
    (cljr--insert-example-fn (cljr--find-symbol-at-point)
                             (if (cljr--is-keyword? keyfn)
                                 (list (s-chop-prefix ":" keyfn))
                               (list 0)))))

(defun cljr--create-fn-from-update-in ()
  (let ((last-path-entry (save-excursion
                           (paredit-backward-down)
                           (cljr--find-symbol-at-point))))
    (cljr--insert-example-fn (cljr--find-symbol-at-point)
                             (if (cljr--is-keyword? last-path-entry)
                                 (list (s-chop-prefix ":" last-path-entry))
                               (list 0)))))

(defun cljr--create-fn-from-sort (sexp-forms)
  (let* ((fn-name (cljr--find-symbol-at-point))
         (param-name (-when-let (coll-name (cljr--guess-param-name (-last-item sexp-forms)))
                       (singularize-string coll-name))))
    (cljr--insert-example-fn fn-name
                             (if param-name
                                 (list (concat param-name "-a")
                                       (concat param-name "-b"))
                               (list "a" "b")))))

(defun cljr--create-fn-from-sort-by (sexp-forms)
  (let* ((fn-name (cljr--find-symbol-at-point))
         (making-comparator? (and (string= fn-name (nth 2 sexp-forms))
                                  (= 4 (length sexp-forms))))
         (param-name (if making-comparator?
                         (when (cljr--is-keyword? (cadr sexp-forms))
                           (s-chop-prefix ":" (cadr sexp-forms)))
                       (-when-let (coll-name (cljr--guess-param-name (-last-item sexp-forms)))
                         (singularize-string coll-name)))))
    (cljr--insert-example-fn fn-name
                             (if making-comparator?
                                 (if param-name
                                     (list (concat param-name "-a")
                                           (concat param-name "-b"))
                                   (list "a" "b"))
                               (list param-name)))))

(defun cljr--create-fn-from-reduce (sexp-forms)
  (cljr--insert-example-fn
   (nth 1 sexp-forms)
   (list (or (and (= 4 (length sexp-forms))
                  (cljr--guess-param-name (nth 2 sexp-forms)))
             "acc")
         (-when-let (name (cljr--guess-param-name (-last-item sexp-forms)))
           (singularize-string name)))))

(defun cljr--unwind-and-extract-this-as-list (name)
  (let* ((parent-sexp (progn
                        (paredit-backward-up)
                        (cljr--extract-sexp)))
         (unwound (cljr--unwind-s parent-sexp)))
    (cljr--with-string-content unwound
      (search-forward (concat "(" name))
      (cljr--extract-sexp-as-list))))

(defun cljr--unwind-s (s)
  (if (s-starts-with? "(->" s)
      (cljr--with-string-content s
        (cljr-unwind-all)
        (buffer-substring (point-min) (point-max)))
    s))

(defun cljr--is-keyword? (s)
  (s-matches? "^:[^0-9:[{(\"][^[{(\"]*$"
              (s-replace "\n" " " s)))

(defun cljr--is-symbol? (s)
  (s-matches? "^[^0-9:[{(\"][^[{(\"]*$"
              (s-replace "\n" " " s)))

(defun cljr--keyword-lookup? (s)
  (string-match "^(:\\([^ 0-9:[{(\"][^[{(\"]+\\) " s))

(defun cljr--first-fn-call-s (s)
  (cljr--with-string-content s
    (when (looking-at "(")
      (paredit-forward-down)
      (cljr--extract-sexp))))

(defun cljr--first-arg-s (s)
  (cljr--with-string-content s
    (paredit-forward-down)
    (paredit-forward)
    (cljr--skip-past-whitespace-and-comments)
    (cljr--extract-sexp)))

(defun cljr--last-arg-s (s)
  (cljr--with-string-content s
    (paredit-forward)
    (paredit-backward-down)
    (paredit-backward)
    (cljr--extract-sexp)))

(defun cljr--guess-param-name (form)
  (let* ((prepped-form (cljr--strip-off-semantic-noops
                        (cljr--unwind-s form)))
         (fn-call (cljr--first-fn-call-s prepped-form)))
    (cond
     ((cljr--is-symbol? prepped-form)
      prepped-form)
     ((cljr--keyword-lookup? prepped-form)
      (match-string 1 prepped-form))
     ((and fn-call (s-ends-with? "." fn-call))
      (s-dashed-words (-last-item (s-split "\\." fn-call t))))
     ((and fn-call (s-starts-with? "create-" fn-call))
      (s-chop-prefix "create-" fn-call))
     ((and fn-call (s-starts-with? ".get" fn-call))
      (s-dashed-words (s-chop-prefix ".get" fn-call)))
     ((string= "get-in" fn-call)
      (cljr--find-param-name-from-get-in prepped-form))
     ((string= "get" fn-call)
      (cljr--find-param-name-from-get prepped-form))
     ((string= "repeat" fn-call)
      (pluralize-string
       (cljr--guess-param-name (cljr--last-arg-s prepped-form))))
     ((member fn-call cljr--fns-that-get-item-out-of-coll)
      (singularize-string
       (cljr--guess-param-name (cljr--first-arg-s prepped-form)))))))

(defvar cljr--fns-that-get-item-out-of-coll
  (list "first" "second" "last" "fnext" "nth" "rand-nth"))

(defun cljr--strip-off-semantic-noops (form)
  "The idea here is that each of these functions, when called on
   something, doesn't truly change what that something is - so we
   can ignore them when trying to figure out a name for a parameter."
  (cljr--with-string-content form
    (let* ((fn-at-point (lambda ()
                          (ignore-errors
                            (save-excursion
                              (paredit-forward-down)
                              (cljr--extract-sexp)))))
           (fn (funcall fn-at-point)))
      (while (or (member fn cljr--semantic-noops--first-position)
                 (member fn cljr--semantic-noops--last-position))
        (if (member fn cljr--semantic-noops--first-position)
            (progn
              (paredit-forward-down)
              (paredit-forward)
              (cljr--skip-past-whitespace-and-comments))
          (paredit-forward)
          (paredit-backward-down)
          (paredit-backward))
        (setq fn (funcall fn-at-point))))
    (cljr--extract-sexp)))

(defvar cljr--semantic-noops--first-position
  (list "assoc" "assoc-in" "update" "update-in" "dissoc" "conj" "concat"
        "cycle" "rest" "nthrest" "nthnext" "next" "nnext" "butlast"
        "reverse" "vec" "set" "distinct"))

(defvar cljr--semantic-noops--last-position
  (list "filter" "filterv" "remove" "take-nth" "cons" "drop" "drop-while"
        "take-last" "take" "take-while" "drop-last" "sort" "sort-by"))

(defun cljr--find-param-name-from-get-in (form)
  (let ((last-path-entry (cljr--with-string-content form
                           (paredit-forward-down)
                           (paredit-forward 3)
                           (paredit-backward-down)
                           (cljr--find-symbol-at-point))))
    (when (cljr--is-keyword? last-path-entry)
      (s-chop-prefix ":" last-path-entry))))

(defun cljr--find-param-name-from-get (form)
  (let ((key (cljr--with-string-content form
               (paredit-forward-down)
               (paredit-forward 2)
               (cljr--skip-past-whitespace-and-comments)
               (cljr--extract-sexp))))
    (when (cljr--is-keyword? key)
      (s-chop-prefix ":" key))))

(defun cljr--insert-example-fn (example-name example-words)
  (let* ((word->arg (lambda (i word)
                      (format "${%s:%s}" (+ i 1)
                              (or (and word (cljr--guess-param-name word))
                                  (format "arg%s" i)))))
         (stub (s-concat (cljr--defn-str)
                         example-name
                         " ["
                         (->> example-words
                              (-map-indexed word->arg)
                              (s-join " "))
                         "]\n$0)")))
    (cljr--make-room-for-toplevel-form)
    (yas-expand-snippet stub)))

(defun cljr--extract-wiki-description (description-buffer)
  (with-current-buffer description-buffer
    (goto-char (point-min))
    (while (not (looking-at-p "<div id=\"wiki-body\""))
      (delete-char 1))
    (sgml-skip-tag-forward 1)
    (buffer-substring (point-min) (point))))

(defun cljr-describe-refactoring (cljr-fn)
  "Show the wiki page, in emacs, for one of the available refactorings."
  (interactive (list (cljr--prompt-user-for "Refactoring to describe: "
                                            (mapcar (lambda (entry) (cadr entry))
                                                    cljr--all-helpers))))
  (let* ((wiki-base-url "https://github.com/clojure-emacs/clj-refactor.el/wiki/")
         (description-buffer "*cljr-describe-refactoring*")
         (description (cljr--extract-wiki-description
                       (url-retrieve-synchronously
                        (concat wiki-base-url cljr-fn)))))
    (pop-to-buffer description-buffer)
    (delete-region (point-min) (point-max))
    (insert description)
    (shr-render-region (point-min) (point-max))
    (view-mode 1)))

(defun cljr--get-function-params (fn)
  "Retrieve the parameters for FN"
  (let* ((info (cider-var-info fn))
         ;; arglists-str looks like this: ([arg] [arg1 arg2] ...)
         (arglists-str (nrepl-dict-get info "arglists-str")))
    (unless arglists-str
      (error "Couldn't retrieve the parameter list for %s" fn))
    (let* ((arglists-str (substring arglists-str 1 -1)))
      (unless (s-matches? "^\\[[^]]+\\]$" arglists-str)
        (error "Can't do work on functions of multiple arities"))
      (s-split " " (substring arglists-str 1 -1)))))

(defvar cljr--change-signature-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-n") #'cljr--move-param-down)
    (define-key keymap (kbd "M-p") #'cljr--move-param-up)
    (define-key keymap (kbd "C-c C-k") #'cljr--abort-signature-edit)
    (define-key keymap (kbd "q") #'cljr--abort-signature-edit)
    (define-key keymap (kbd "e") #'cljr--edit-parameter-name)
    (define-key keymap (kbd "C-c C-c") #'cljr--commit-signature-edit)
    (define-key keymap (kbd "RET") #'cljr--commit-signature-edit)
    keymap))

(defun cljr--abort-signature-edit ()
  (interactive)
  (kill-buffer cljr--change-signature-buffer))

(defun cljr--move-line-up ()
  "Move the current line up."
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun cljr--move-line-down ()
  "Move the current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun cljr--dec-parameter-index ()
  (let* ((index (1- (line-number-at-pos)))
         (parameter-info (nth index cljr--signature-changes))
         (neighbor-info (nth (1- index) cljr--signature-changes)))
    (puthash :new-index (1- (gethash :new-index parameter-info))
             parameter-info)
    (puthash :new-index (1+ (gethash :new-index neighbor-info))
             neighbor-info)))

(defun cljr--inc-parameter-index ()
  (let* ((index (1- (line-number-at-pos)))
         (parameter-info (nth index cljr--signature-changes))
         (neighbor-info (nth (1+ index) cljr--signature-changes)))
    (puthash :new-index (1+ (gethash :new-index parameter-info))
             parameter-info)
    (puthash :new-index (1- (gethash :new-index neighbor-info))
             neighbor-info)))

(defun cljr--move-param-down ()
  (interactive)
  (unless (save-excursion (beginning-of-line) (or (looking-at-p "\\s-*$")
                                                  (looking-at-p "#")))
    (when (save-excursion (beginning-of-line) (looking-at-p "& "))
      (error "Can't move the rest parameter!"))
    (view-mode -1)
    (cljr--move-line-down)
    (view-mode 1)
    (cljr--dec-parameter-index)))

(defun cljr--move-param-up ()
  (interactive)
  (unless (or (= (line-number-at-pos) 1)
              (or (looking-at-p "\\s-*$")
                  (looking-at-p "#")))
    (when (save-excursion (beginning-of-line) (looking-at-p "& "))
      (error "Can't move the rest parameter!"))
    (view-mode -1)
    (cljr--move-line-up)
    (cljr--inc-parameter-index)
    (view-mode 1)))

(defun cljr--edit-parameter-name ()
  (interactive)
  (let* ((index (1- (line-number-at-pos)))
         (new-name (read-from-minibuffer
                    "New parameter name: "
                    (save-excursion
                      (beginning-of-line)
                      (buffer-substring (point)
                                        (cljr--point-after 'paredit-forward)))))
         (parameter-info (nth index cljr--signature-changes)))
    (puthash :new-name new-name parameter-info)
    (view-mode -1)
    (delete-region (point-at-bol) (point-at-eol))
    (insert new-name)
    (view-mode 1)))

(defun cljr--defn? (occurrence)
  (s-matches? (rx (seq line-start (* whitespace) "("
                       (? (+ (or (in "a-z") (in "A-z") (in "0-9")
                                 (in "-") (in "._/"))))
                       "defn"))
              (plist-get occurrence :match)))

(defun cljr--update-parameter-name (new-name)
  (cljr--skip-past-whitespace-and-comments)
  (forward-char)
  (cljr-rename-symbol new-name))

(defun cljr--forward-parameter ()
  "Move point forward across one parameter.

This includes skipping past any type information added by
prismatic/schema and moving paste any whitespace"
  (paredit-forward)
  (cljr--skip-past-whitespace-and-comments)
  (when (looking-at-p ":-")
    (paredit-forward 2))
  (cljr--skip-past-whitespace-and-comments))

(defun cljr--update-signature-names (signature-changes)
  "Point is assumed to be at the the first character in the
  lambda list.

Updates the names of the function parameters."
  (dolist (changes signature-changes)
    (unless (string= (gethash :new-name changes)
                     (gethash :old-name changes))
      (cljr--update-parameter-name (gethash :new-name changes)))
    (cljr--forward-parameter)))

(defun cljr--delete-and-extract-region (beg end)
  (prog1
      (buffer-substring-no-properties beg end)
    (delete-region beg end)))

(defun cljr--delete-and-extract-function-parameter ()
  (cljr--skip-past-whitespace-and-comments)
  (let (parameter)
    (push (cljr--delete-and-extract-region
           (point) (cljr--point-after 'paredit-forward))
          parameter)
    (delete-region (point) (cljr--point-after
                            'cljr--skip-past-whitespace-and-comments))
    (when (looking-at-p ":-")
      (push (cljr--delete-and-extract-region
             (point) (cljr--point-after '(paredit-forward 2)))
            parameter))
    (delete-region (point) (cljr--point-after
                            'cljr--skip-past-whitespace-and-comments))
    (s-join " " (nreverse parameter))))

(defun cljr--maybe-wrap-form ()
  "Insert newlines in or prior to the current form to prevent long lines.

Point is assumed to be at the end of the form."
  (let ((breakpoint (or (and (boundp 'whitespace-line-column)
                             whitespace-line-column)
                        80)))
    (when (> (current-column) breakpoint)
      (paredit-backward-up)
      (if (and (not (looking-back "^\\s-*")) (looking-at-p "\\["))
          (newline-and-indent) ; Put lambdalist on its own line
        (paredit-forward-down)
        (cljr--forward-parameter) ; don't break right after ( or [
        (while (save-excursion (cljr--forward-parameter)
                               (< (current-column) breakpoint))
          (cljr--forward-parameter))
        (newline-and-indent)))))

(defun cljr--update-signature-order (signature-changes)
  "Point is assumed to be at the first character in the lambda list.

Updates the ordering of the function parameters."
  (unless (-every? (lambda (c) (= (gethash :new-index c) (gethash :old-index c)))
                   signature-changes)
    (let (parameters)
      ;; extract parameters
      (dolist (_ signature-changes)
        (push (cljr--delete-and-extract-function-parameter)
              parameters))
      (setq parameters (nreverse parameters))
      ;; leave point in empty lambda list
      (paredit-backward-up)
      (paredit-forward-down)
      (delete-region (point) (cljr--point-after 'cljr--skip-past-whitespace-and-comments))
      ;; insert parameters in new order
      (dotimes (i (length parameters))
        (let ((old-name (gethash :old-name
                                 (-find (lambda (c) (= (gethash :new-index c) i))
                                        signature-changes))))
          (insert (-find (lambda (param)
                           (s-starts-with? old-name param))
                         parameters)))
        (unless (= (1+ i) (length parameters))
          (insert " ")))
      (cljr--maybe-wrap-form))))

(defun cljr--update-function-signature (signature-changes)
  "Point is assumed to be just prior to the function definition
  we're about to update."
  (paredit-forward-down 2)
  (cljr--update-signature-names signature-changes)
  (cljr--goto-toplevel)
  (paredit-forward-down 2)
  (cljr--update-signature-order signature-changes))

(defun cljr--call-site? (fn)
  "Is point at a call-site for FN?"
  (save-excursion
    (ignore-errors
      (paredit-backward-up)
      (paredit-forward-down)
      (s-ends-with? (cljr--symbol-suffix fn) (cider-symbol-at-point)))))

(defun cljr--no-changes-to-parameter-order? (signature-changes)
  (-every? (lambda (e) (= (gethash :new-index e) (gethash :old-index e)))
           signature-changes))

(defun cljr--update-call-site (signature-changes)
  "Point is assumed to be at the name of the function being
called."
  (unless (cljr--no-changes-to-parameter-order? signature-changes)
    (cljr--forward-parameter)
    (let (args)
      (dotimes (_ (length signature-changes))
        (push (cljr--delete-and-extract-function-parameter) args))
      (setq args (nreverse args))
      (dotimes (i (length args))
        (insert (nth (gethash :old-index
                              (-find (lambda (c) (= (gethash :new-index c) i))
                                     signature-changes))
                     args))
        (unless (= (1+ i) (length args))
          (insert " ")))
      (cljr--maybe-wrap-form))))

(defun cljr--append-to-manual-intervention-buffer ()
  "Append the current line to the buffer of stuff requiring
manual intervention."
  (let ((line (s-trim (buffer-substring-no-properties
                       (point-at-bol) (point-at-eol))))
        (linum (line-number-at-pos))
        (file (buffer-file-name)))
    (with-current-buffer (get-buffer-create cljr--manual-intervention-buffer)
      (goto-char (point-max))
      (insert (format "%s:%s: %s\n" file linum line)))))

(defun cljr--update-apply-call-site (signature-changes)
  "Update a call-site where apply is used to call the function
  whose signature we're currently editing.

point is assumed to be at the function name"
  (unless (cljr--no-changes-to-parameter-order? signature-changes)
    (let ((num-args 0)
          (max-index (->> signature-changes
                          (-map (lambda (c) (let ((new  (gethash :new-index c))
                                                  (old (gethash :old-index c)))
                                              (if (/= old new)
                                                  (max old new)))))
                          (-remove #'null)
                          (apply #'max)))
          beg end)
      (cljr--skip-past-whitespace-and-comments)
      (setq beg (point))
      (paredit-forward)
      (setq end (cljr--point-after 'paredit-forward-up))
      (while (< (save-excursion (cljr--point-after 'cljr--forward-parameter)) end)
        (cljr--forward-parameter)
        (setq num-args (1+ num-args)))
      (if (>=  max-index (1- num-args))
          ;; Some of the arguments in the final list of args to apply have changed
          (cljr--append-to-manual-intervention-buffer)
        (goto-char beg)
        (cljr--update-call-site signature-changes)))))

(defun cljr--update-partial-call-site (signature-changes)
  "Update a call-site with partial application of the function
  whose signature we're currently editing.

This only handles the case where we have (partial my-fn a b c)
and only parameters a b or c are affected.

point is assumed to be at the function name"
  (unless (cljr--no-changes-to-parameter-order? signature-changes)
    (let ((num-partials 0)
          (max-index (->> signature-changes
                          (-map (lambda (c) (let ((new  (gethash :new-index c))
                                                  (old (gethash :old-index c)))
                                              (when (/= old new)
                                                (max old new)))))
                          (-remove #'null)
                          (apply #'max)))
          beg end)
      (setq beg (point))
      (cljr--skip-past-whitespace-and-comments)
      (paredit-forward 1)
      (setq end (cljr--point-after 'paredit-forward-up))
      (while (< (save-excursion (cljr--point-after 'cljr--forward-parameter)) end)
        (cljr--forward-parameter)
        (setq num-partials (1+ num-partials)))
      (if (>=  max-index num-partials)
          (cljr--append-to-manual-intervention-buffer)
        (goto-char beg)
        (cljr--update-call-site (-remove (lambda (c)
                                           (>= (gethash :new-index c) num-partials))
                                         signature-changes))))))

(defun cljr--apply-call-site? ()
  "Is the function invocation at this place being done using
  apply?

Point is assumed to be at the function being called."
  (ignore-errors
    (save-excursion
      (paredit-backward-up)
      (paredit-forward-down)
      (looking-at-p "apply"))))

(defun cljr--partial-call-site? ()
  "Is the function invocation at this place being done using
  partial.

Point is assumed to be at the function being called."
  (ignore-errors
    (save-excursion
      (paredit-backward-up)
      (paredit-forward-down)
      (looking-at-p "partial"))))

(defun cljr--ignorable-occurrence? ()
  (save-excursion
    (cljr--goto-toplevel)
    (looking-at-p "\\s-*(ns")))

(defun cljr--change-function-signature (occurrences signature-changes)
  ;; SIGNATURE-CHANGES is a list of hashmaps with keys:
  ;; :old-index, :new-index, :old-name :new-name
  ;; Indexing is from 0
  ;; The OCCURRENCES are the same as those returned by `cljr--find-symbol'
  (dolist (symbol-meta occurrences)
    (let ((file (plist-get symbol-meta :file))
          (line-beg (plist-get symbol-meta :line-beg))
          (col-beg (plist-get symbol-meta :col-beg))
          (name (plist-get symbol-meta :name)))
      (with-current-buffer
          (find-file-noselect file)
        (goto-char (point-min))
        (forward-line (1- line-beg))
        (move-to-column (1- col-beg))
        (cond
         ((cljr--ignorable-occurrence?) :do-nothing)
         ((cljr--call-site? name) (cljr--update-call-site signature-changes))
         ((cljr--partial-call-site?)
          (cljr--update-partial-call-site signature-changes))
         ((cljr--apply-call-site?)
          (cljr--update-apply-call-site signature-changes))
         ((cljr--defn? symbol-meta)
          (cljr--update-function-signature signature-changes))
         (t (cljr--append-to-manual-intervention-buffer)))
        (save-buffer))))
  (unless (cljr--empty-buffer? (get-buffer-create cljr--manual-intervention-buffer))
    (pop-to-buffer cljr--manual-intervention-buffer)
    (goto-char (point-min))
    (insert "The following occurrence(s) couldn't be handled automatically:\n\n")
    (grep-mode)
    (setq-local compilation-search-path (list (cljr--project-dir)))))

(defun cljr--commit-signature-edit ()
  (interactive)
  (cljr--change-function-signature cljr--occurrences cljr--signature-changes)
  (kill-buffer cljr--change-signature-buffer))

(define-derived-mode cljr--change-signature-mode fundamental-mode
  "Change Signature"
  "Major mode for refactoring function signatures.")

(defun cljr--setup-change-signature-buffer (control-buffer params)
  (when (get-buffer control-buffer)
    (kill-buffer control-buffer))
  (pop-to-buffer control-buffer)
  (delete-region (point-min) (point-max))
  (insert "

# M-n and M-p to re-order parameters.
# e or C-c C-e to edit a name.
# RET or C-c C-c when you're happy with your changes.
# q or C-c C-k to abort. ")
  (goto-char (point-min))
  (insert (s-join "\n" params))
  (forward-line -1)
  (when (looking-at-p "&")
    (forward-line 1)
    (join-line))
  (setq cljr--signature-changes (let (signature-changes)
                                  (dotimes (i (length params))
                                    (let ((h (make-hash-table)))
                                      (puthash :old-index i h)
                                      (puthash :new-index i h)
                                      (puthash :old-name (nth i params) h)
                                      (puthash :new-name (nth i params) h)
                                      (push h signature-changes)))
                                  (nreverse signature-changes)))
  (cljr--change-signature-mode)
  (view-mode))

;;;###autoload
(defun cljr-change-function-signature ()
  "Change the function signature of the function at POINT.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-change-function-signature"
  (interactive)
  (let* ((fn (cider-symbol-at-point))
         (params (cljr--get-function-params fn))
         (var-info (cider-var-info fn))
         (ns (nrepl-dict-get var-info "ns")))
    (setq cljr--occurrences (cljr--find-symbol-sync fn ns)
          cljr--signature-changes nil)
    (cljr--setup-change-signature-buffer cljr--change-signature-buffer params)
    (when (get-buffer cljr--manual-intervention-buffer)
      (kill-buffer cljr--manual-intervention-buffer))
    (pop-to-buffer cljr--change-signature-buffer)))

(add-hook 'nrepl-connected-hook #'cljr--init-middleware)

;; ------ minor mode -----------
;;;###autoload
(define-minor-mode clj-refactor-mode
  "A mode to keep the clj-refactor keybindings."
  nil " cljr" clj-refactor-map)

;; Deprecated
(defun cljr-update-artifact-cache ()
  (interactive)
  (message "cljr-update-artifact-cache is deprecated and has been replaced by a customize setting defaulting to true."))

(defun cljr-rename-file ()
  (interactive)
  (message "cljr-rename-file has been removed in favor of cljr-rename-file-or-dir"))

(defun cljr-warm-ast-cache ()
  (interactive)
  (message "cljr-warm-ast-cache has been deprecated and replaced by a defcustom defaulting to true."))

(provide 'clj-refactor)
;;; clj-refactor.el ends here
