;;; clj-refactor.el --- A collection of clojure refactoring functions

;; Copyright © 2012-2014 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.13.0
;; Keywords: convenience
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (yasnippet "0.6.1") (paredit "22") (multiple-cursors "1.2.2") (cider "0.8.1"))

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

(defcustom cljr-add-ns-to-blank-clj-files t
  "When true, automatically adds a ns form to new clj files."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-sort-comparator 'cljr--string-natural-comparator
  "The comparator function to use to sort ns declaration. Set your
   own if you see fit. Comparator is called with two elements of
   the sub section of the ns declaration, and should return non-nil
   if the first element should sort before the second."
  :group 'cljr
  :type 'function)

(defcustom cljr-auto-sort-ns t
  "When true, sort ns form whenever adding to the form using clj-refactor
   functions."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-magic-requires t
  "When true, suggests requiring common namespaces when you type
  its short form. Set to :prompt to ask before doing anything."
  :group 'cljr
  :type '(choice (const :tag "true" t)
                 (const :tag "prompt" :prompt)
                 (const :tag "false" nil)))

(defcustom cljr-use-metadata-for-privacy nil
  "When nil, `cljr-cycle-privacy' will use (defn- f []).
   When t, it will use (defn ^:private f [])"
  :group 'cljr
  :type 'boolean)

(defcustom cljr-project-clean-prompt t
  "When true prompts to ask before doing anything if false
   runs project clean functions without warning."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-find-symbols-in-dir-prompt nil
  "When true prompts for directory to search for symbols in when
   finding usages and renaming symbols, when false defaults to project
   dir"
  :group 'cljr
  :type 'boolean)

(defcustom cljr-project-clean-functions
  (list 'cljr-remove-unused-requires 'cljr-sort-ns)
  "List of functions to run on all the clj files in the project
   when you perform project clean."
  :group 'cljr
  :type '(repeat function))

(defcustom cljr-project-clean-exceptions '("dev/user.clj")
  "Contains a list of files that should not be cleaned when
  running `cljr-project-clean'."
  :group 'cljr
  :type '(repeat string))

(defcustom cljr-debug-functions "println,pr,prn"
  "List of functions used for debug purposes.
Used in `cljr-remove-debug-fns' feature."
  :group 'cljr
  :type 'string)

(defcustom cljr-hotload-dependencies t
  "When true new dependencies added with
`cljr-add-project-dependency' are also hotloaded into the repl.'"
  :group 'cljr
  :type 'boolean)

(defvar cljr-magic-require-namespaces
  '(("io"   . "clojure.java.io")
    ("set"  . "clojure.set")
    ("str"  . "clojure.string")
    ("walk" . "clojure.walk")
    ("zip"  . "clojure.zip")))

(defvar clj-refactor-map (make-sparse-keymap) "")

(defvar cljr--add-require-snippet "${1:[${2:$3 :as $4}]}"
  "The snippet used in in `cljr-add-require-to-ns'")

(defvar cljr--add-use-snippet "[$1 :refer ${2:[$3]}]"
  "The snippet used in in `cljr-add-use-to-ns'")

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

(defun cljr--add-keybindings (key-fn)
  (define-key clj-refactor-map (funcall key-fn "ad") 'cljr-add-declaration)
  (define-key clj-refactor-map (funcall key-fn "ai") 'cljr-add-import-to-ns)
  (define-key clj-refactor-map (funcall key-fn "ap") 'cljr-add-project-dependency)
  (define-key clj-refactor-map (funcall key-fn "am") 'cljr-add-missing-libspec)
  (define-key clj-refactor-map (funcall key-fn "ar") 'cljr-add-require-to-ns)
  (define-key clj-refactor-map (funcall key-fn "au") 'cljr-add-use-to-ns)
  (define-key clj-refactor-map (funcall key-fn "cn") 'cljr-clean-ns)
  (define-key clj-refactor-map (funcall key-fn "cc") 'cljr-cycle-coll)
  (define-key clj-refactor-map (funcall key-fn "ci") 'cljr-cycle-if)
  (define-key clj-refactor-map (funcall key-fn "cp") 'cljr-cycle-privacy)
  (define-key clj-refactor-map (funcall key-fn "cs") 'cljr-cycle-stringlike)
  (define-key clj-refactor-map (funcall key-fn "ct") 'cljr-cycle-thread)
  (define-key clj-refactor-map (funcall key-fn "dk") 'cljr-destructure-keys)
  (define-key clj-refactor-map (funcall key-fn "el") 'cljr-expand-let)
  (define-key clj-refactor-map (funcall key-fn "ef") 'cljr-extract-function)
  (define-key clj-refactor-map (funcall key-fn "fu") 'cljr-find-usages)
  (define-key clj-refactor-map (funcall key-fn "hd") 'cljr-hotload-dependency)
  (define-key clj-refactor-map (funcall key-fn "il") 'cljr-introduce-let)
  (define-key clj-refactor-map (funcall key-fn "mf") 'cljr-move-form)
  (define-key clj-refactor-map (funcall key-fn "ml") 'cljr-move-to-let)
  (define-key clj-refactor-map (funcall key-fn "pc") 'cljr-project-clean)
  (define-key clj-refactor-map (funcall key-fn "pf") 'cljr-promote-function)
  (define-key clj-refactor-map (funcall key-fn "rf") 'cljr-rename-file)
  (define-key clj-refactor-map (funcall key-fn "rl") 'cljr-remove-let)
  (define-key clj-refactor-map (funcall key-fn "rs") 'cljr-rename-symbol)
  (define-key clj-refactor-map (funcall key-fn "rr") 'cljr-remove-unused-requires)
  (define-key clj-refactor-map (funcall key-fn "ru") 'cljr-replace-use)
  (define-key clj-refactor-map (funcall key-fn "sn") 'cljr-sort-ns)
  (define-key clj-refactor-map (funcall key-fn "sp") 'cljr-sort-project-dependencies)
  (define-key clj-refactor-map (funcall key-fn "sr") 'cljr-stop-referring)
  (define-key clj-refactor-map (funcall key-fn "tf") 'cljr-thread-first-all)
  (define-key clj-refactor-map (funcall key-fn "th") 'cljr-thread)
  (define-key clj-refactor-map (funcall key-fn "tl") 'cljr-thread-last-all)
  (define-key clj-refactor-map (funcall key-fn "ua") 'cljr-unwind-all)
  (define-key clj-refactor-map (funcall key-fn "uw") 'cljr-unwind)
  (define-key clj-refactor-map (funcall key-fn "rd") 'cljr-remove-debug-fns))

;;;###autoload
(defun cljr-add-keybindings-with-prefix (prefix)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-prefix prefix)))

;;;###autoload
(defun cljr-add-keybindings-with-modifier (modifier)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-modifier modifier)))

;; ------ utilities -----------

(defun cljr--delete-and-extract-sexp ()
  (let* ((beg (point))
         (end (cljr--point-after 'paredit-forward))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    contents))

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
    (paredit-forward-down)
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
  (let ((depth (first (paredit-current-parse-state))))
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

;; ------ file -----------

(defun cljr--project-dir ()
  (or (ignore-errors
        (file-truename
         (locate-dominating-file default-directory "project.clj")))
      (ignore-errors (file-truename
                      (locate-dominating-file default-directory "pom.xml")))))

(defun cljr--project-file ()
  (or (ignore-errors
        (expand-file-name "project.clj" (cljr--project-dir)))
      (ignore-errors (expand-file-name "pom.xml" (cljr--project-dir)))))

(defun cljr--project-files ()
  (split-string (shell-command-to-string
                 (format "find %s -type f \\( %s \\) %s | head -n %s"
                         (cljr--project-dir)
                         (format "-name \"%s\"" "*.clj")
                         "-not -regex \".*svn.*\""
                         1000))))

(defun cljr--rename-file (filename new-name)
  (let ((old-ns (clojure-find-ns)))
    (rename-file filename new-name 1)
    (rename-buffer new-name)
    (set-visited-file-name new-name)
    (clojure-update-ns)
    (let ((old-syntax (char-to-string (char-syntax ?/))))
      (modify-syntax-entry ?/ " ")
      (save-window-excursion
        (save-excursion
          (ignore-errors
            (tags-query-replace (concat (regexp-quote old-ns) "\\_>")
                                (clojure-expected-ns) nil
                                '(cljr--project-files)))))
      (modify-syntax-entry ?/ old-syntax))
    (save-buffer)
    (save-some-buffers)))

;;;###autoload
(defun cljr-rename-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (cljr--rename-file filename new-name)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

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

(defun cljr--add-test-use-declarations ()
  (save-excursion
    (let ((ns (clojure-find-ns)))
      (cljr--insert-in-ns ":require")
      (insert "[" (s-chop-suffix "-test" ns) " :refer :all]")
      (cljr--insert-in-ns ":require")
      (insert "[" (if (cljr--project-depends-on-p "midje")
                      "midje.sweet"
                    "clojure.test")
              " :refer :all]"))))

(defun cljr--in-tests-p ()
  "Check whether the current file is a test file.

Two checks are made - whether the namespace of the file has the
word test in it and whether the file lives under the test/ directory."
  (or (string-match-p "test\." (clojure-find-ns))
      (string-match-p "/test" (buffer-file-name))))

(defun cljr--add-ns-if-blank-clj-file ()
  (ignore-errors
    (when (and cljr-add-ns-to-blank-clj-files
               (or (s-ends-with? ".clj" (buffer-file-name))
                   (s-ends-with? ".cljs" (buffer-file-name))
                   (s-ends-with? ".cljx" (buffer-file-name)))
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
           (s-matches? "-[^/]+\.clj$" file-name)
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
  (if (not (cljr--search-forward-within-sexp (concat "(" statement-type)))
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
  (interactive)
  (save-excursion
    (let ((comparator (cljr-create-comparator cljr-sort-comparator)))
      (dolist (statement-type '(":require" ":use" ":import"))
        (ignore-errors
          (dolist (statement (->> (cljr--extract-ns-statements statement-type nil)
                                  (-map 's-trim)
                                  (-sort comparator)
                                  (-distinct)))
            (cljr--insert-in-ns statement-type)
            (insert statement)))))))

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
  (interactive)
  (cljr--assert-middleware)
  (let* ((body (replace-regexp-in-string "\"" "\"" (buffer-substring-no-properties (point-min) (point-max))))
         (result (cljr--call-middleware-sync "value"
                                             (list "op" "refactor"
                                                   "ns-string" body
                                                   "refactor-fn" "find-debug-fns"
                                                   "debug-fns" cljr-debug-functions)))
         (debug-fn-tuples (pop result))
         (removed-lines 0))
    (while debug-fn-tuples
      (let ((line (- (1- (car debug-fn-tuples)) removed-lines))
            (end-line (nth 1 debug-fn-tuples))
            (column (nth 2 debug-fn-tuples)))
        (message "removing %s at line %s [%s] column %s (end-line %s end-column %s)" (-last-item debug-fn-tuples) line (car debug-fn-tuples) column end-line (nth 3 debug-fn-tuples))
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

;;;###autoload
(defun cljr-remove-unused-requires ()
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
      (indent-region beg end)
      (when cljr-auto-sort-ns
        (cljr-sort-ns)))))

(defvar cljr--tmp-marker (make-marker))

(defun cljr--pop-tmp-marker-after-yasnippet-1 (&rest ignore)
  (goto-char cljr--tmp-marker)
  (set-marker cljr--tmp-marker nil)
  (remove-hook 'yas/after-exit-snippet-hook 'cljr--pop-tmp-marker-after-yasnippet-1 t))

(defun cljr--pop-tmp-marker-after-yasnippet ()
  (add-hook 'yas/after-exit-snippet-hook 'cljr--pop-tmp-marker-after-yasnippet-1 nil t))

(defun cljr--sort-and-remove-hook (&rest ignore)
  (cljr-sort-ns)
  (remove-hook 'yas/after-exit-snippet-hook 'cljr--pop-tmp-marker-after-yasnippet-1 t))

(defun cljr--add-yas-snippet-sort-ns-hook ()
  (add-hook 'yas/after-exit-snippet-hook 'cljr--sort-and-remove-hook nil t))

;;;###autoload
(defun cljr-add-require-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas-expand-snippet cljr--add-require-snippet))

;;;###autoload
(defun cljr-add-use-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas-expand-snippet cljr--add-use-snippet))

;;;###autoload
(defun cljr-add-import-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":import")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas-expand-snippet "$1"))

(defun cljr--extract-ns-from-use ()
  "Let point be denoted by |.  Then, when called on: |[used.ns ...]
returns used.ns, when called on (:use some.ns) returns some.ns"
  (let* ((form (format "%s" (sexp-at-point))))
    (if (looking-at "(:use [A-z.0-9-]+)")
        (s-chop-suffix ")" (second (s-split " " form)))
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

Presently, there's no support for :use clauses containing :exclude."
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
  (when cljr-auto-sort-ns
    (cljr-sort-ns)))

;;;###autoload
(defun cljr-stop-referring ()
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
        (kill-sexp 2)
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

;;;###autoload
(defun cljr-move-form ()
  "Move the form containing POINT to a new namespace.

If REGION is active, move all forms contained by region. "
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
        (open-line 2)
        (forward-line 2)
        (insert forms)
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
        (cljr--new-require-clause ns refer-names))
      (when cljr-auto-sort-ns
        (cljr-sort-ns)))))

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

(defun cljr--name-of-defns (string-with-defns &optional include-private)
  "Returns a list of the function names in STRING-WITH-DEFNS,
optionally including those that are declared private."
  (with-temp-buffer
    (insert string-with-defns)
    (goto-char (point-min))
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

;;;###autoload
(defun cljr-add-declaration ()
  (interactive)
  (save-excursion
    (-if-let (def (cljr--name-of-current-def))
        (progn (cljr--goto-declare)
               (backward-char)
               (insert " " def))
      (message "Not inside a def form."))))

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
  (interactive)
  (ignore-errors
    (forward-char 3))
  (search-backward-regexp "([^-]*->")
  (if (cljr--nothing-more-to-unwind)
      (cljr--pop-out-of-threading)
    (paredit-forward-down)
    (cond
     ((looking-at "[^-]*->[\n\r\t ]")  (cljr--unwind-first))
     ((looking-at "[^-]*->>[\n\r\t ]") (cljr--unwind-last)))
    t))

;;;###autoload
(defun cljr-unwind-all ()
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
        (message "Nothing more to thread.")
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
    (if (looking-back "(" 1)
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
  (interactive)
  (save-excursion
    (paredit-wrap-round)
    (insert "-> "))
  (while (save-excursion (cljr-thread))
    t))

;;;###autoload
(defun cljr-thread-last-all ()
  (interactive)
  (save-excursion
    (paredit-wrap-round)
    (insert "->> "))
  (while (save-excursion (cljr-thread))
    t))

;; ------ let binding ----------

;;;###autoload
(defun cljr-introduce-let ()
  (interactive)
  (paredit-wrap-round)
  (insert "let ")
  (paredit-wrap-square)
  (insert " ")
  (backward-char)
  (mc/create-fake-cursor-at-point)
  (paredit-forward-up)
  (newline-and-indent)
  (mc/maybe-multiple-cursors-mode))

(add-to-list 'mc--default-cmds-to-run-once 'cljr-introduce-let)

(defun cljr--goto-let ()
  (while (not (or (cljr--toplevel-p)
                  (looking-at "\(\\(when-let\\|if-let\\|let\\)\\( \\|\\[\\)")))
    (paredit-backward-up)))

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

;;;###autoload
(defun cljr-expand-let ()
  (interactive)
  (ignore-errors
    (forward-char 4))
  (cljr--goto-let)
  (paredit-forward-down 2)
  (paredit-forward-up)
  (skip-syntax-forward " >")
  (paredit-convolute-sexp)
  (-each (cljr--get-let-bindings) 'cljr--replace-sexp-with-binding))

(defun cljr--replace-sexp-with-binding-in-let ()
  (remove-hook 'multiple-cursors-mode-disabled-hook 'cljr--replace-sexp-with-binding-in-let)
  (-each (cljr--get-let-bindings) 'cljr--replace-sexp-with-binding))

;;;###autoload
(defun cljr-move-to-let ()
  (interactive)
  (save-excursion
    (let ((contents (cljr--delete-and-extract-sexp)))
      (cljr--prepare-to-insert-new-let-binding)
      (insert contents))
    (backward-sexp)
    (insert " ")
    (backward-char)
    (mc/create-fake-cursor-at-point))
  (add-hook 'multiple-cursors-mode-disabled-hook 'cljr--replace-sexp-with-binding-in-let)
  (mc/maybe-multiple-cursors-mode))

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
  (save-excursion
    (let ((pos (point)))
      (cljr--goto-let)
      (re-search-forward "\\[")
      (if (< pos (point))
          nil
        (paredit-forward-up)
        (< pos (point))))))

(defun cljr--depth-of-let-bindings ()
  "Returns the depth where the variable bindings for the active
let are."
  (save-excursion
    (cljr--goto-let)
    (re-search-forward "\\[")
    (cljr--depth-at-point)))

(defun cljr--eliminate-let ()
  (cljr--goto-let)
  (paredit-forward-down)
  (paredit-forward 2)
  (paredit-splice-sexp-killing-backward))

(defun cljr-remove-let ()
  "Inlines all variables in the let form and removes it."
  (interactive)
  (cljr--goto-let)
  (search-forward "[")
  (paredit-forward-up)
  (let* ((beg (point))
         (end (cljr--point-after '(paredit-forward-up 1)))
         (bindings (cljr--get-let-bindings)))

    (dolist (binding bindings)
      (replace-regexp (first binding) (second binding) :delimited beg end)
      (cljr--goto-let)
      (setq end (cljr--point-after 'paredit-forward)))

    (cljr--eliminate-let)))


(add-to-list 'mc--default-cmds-to-run-once 'cljr-move-to-let)

;; ------ Destructuring ----

(defun cljr--find-symbol-at-point ()
  (save-excursion
    (when (looking-back "\\s_\\|\\sw" 1)
      (paredit-backward))
    (let ((beg (point)))
      (paredit-forward)
      (buffer-substring-no-properties beg (point)))))

;;;###autoload
(defun cljr-destructure-keys ()
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
    (when (looking-back "\\s_\\|\\sw" 1)
      (paredit-backward))
    (kill-sexp)
    (insert "{:keys [" (s-join " " (-distinct (reverse symbols))) "]"
            (if include-as (concat " :as " symbol) "") "}")))

;; ------ Cycling ----------

;;;###autoload
(defun cljr-cycle-privacy ()
  (interactive)
  (save-excursion
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
(defun cljr-cycle-stringlike ()
  "Removed, use `clojure-toggle-keyword-string'"
  (interactive)
  (message "Removed, use `clojure-toggle-keyword-string'"))

;;;###autoload
(defun cljr-cycle-coll ()
  "Convert the coll at (point) from (x) -> {x} -> [x] -> -> #{x} -> (x) recur"
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
  "Cycle surrounding if or if-not, to if-not or if"
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
  (when (looking-back " #" 2)
    (delete-char -1)))

;;;###autoload
(defun cljr-splice-sexp-killing-backward (&optional argument)
  "Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets."
  (interactive "P")
  (paredit-splice-sexp-killing-backward argument)
  (when (looking-back " #" 2)
    (delete-char -1)))

;;;###autoload
(defun cljr-splice-sexp-killing-forward (&optional argument)
  "Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets."
  (interactive "P")
  (save-excursion
    (paredit-backward-up)
    (when (looking-back " #" 2)
      (delete-char -1)))
  (paredit-splice-sexp-killing-forward argument))

;; ------ magic requires -------

(defun cljr--magic-requires-re ()
  (concat "(\\(" (regexp-opt (-map 'car cljr-magic-require-namespaces)) "\\)/"))

;;;###autoload
(defun cljr-slash ()
  "Inserts / as normal, but also checks for common namespace shorthands to require."
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
            (when cljr-auto-sort-ns
              (cljr-sort-ns)))))))

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
  "Runs `cljr-project-clean-functions' on every clojure file, then
sorts the project's dependency vectors."
  (interactive)
  (when (or (not cljr-project-clean-prompt)
            (yes-or-no-p "Cleaning your project might change many of your clj files. Do you want to proceed?"))
    (dolist (filename (cljr--project-files))
      (when (and (s-ends-with? "clj" filename)
                 (not (cljr--excluded-from-project-clean? filename)))
        (cljr--update-file filename
                           (ignore-errors (-map 'funcall cljr-project-clean-functions)))))
    (cljr-sort-project-dependencies)
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

(defun cljr--empty-buffer? ()
  (s-blank? (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

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
  (interactive)
  "Sorts all dependency vectors in project.clj"
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

(defun cljr--call-middleware-sync (key request)
  (let ((nrepl-sync-request-timeout 25))
    (nrepl-dict-get (nrepl-send-sync-request request) key)))

(defun cljr--call-middleware-async (request &optional callback)
  (nrepl-send-request request callback))

(defun cljr--get-artifacts-from-middleware (force)
  (message "Retrieving list of available libraries...")
  (let ((request (list "op" "artifact-list" "force" (if force "true" "false"))))
    (->> request
         (cljr--call-middleware-sync "value")
         (s-split " "))))

(defun cljr-update-artifact-cache ()
  (interactive)
  (cljr--call-middleware-async (list "op" "artifact-list"
                                     "force" "true")
                               (lambda (_) (message "Artifact cache updated"))))

(defun cljr--get-versions-from-middleware (artifact)
  (let ((request (list "op" "artifact-versions"
                       "artifact" artifact)))
    (->> request
         (cljr--call-middleware-sync "value")
         (s-split " "))))

(defun cljr--prompt-user-for (prompt &optional choices)
  (completing-read prompt choices))

(defun cljr--add-project-dependency (artifact version)
  (save-window-excursion
    (find-file (cljr--project-file))
    (goto-char (point-min))
    (re-search-forward ":dependencies")
    (paredit-forward)
    (paredit-backward-down)
    (newline-and-indent)
    (insert "[" artifact " \"" version "\"]")
    (save-buffer)
    (message "Added %s version %s as a project dependency" artifact version)
    (when cljr-hotload-dependencies
      (paredit-backward-down)
      (cljr-hotload-dependency))))

(defun cljr--assert-middleware ()
  (unless (featurep 'cider)
    (error "CIDER isn't installed!"))
  (unless (cider-connected-p)
    (error "CIDER isn't connected!"))
  (unless (nrepl-op-supported-p "refactor")
    (error "nrepl-refactor middleware not available!")))

(defun cljr--assert-leiningen-project ()
  (unless (string= (file-name-nondirectory (or (cljr--project-file) ""))
                   "project.clj")
    (error "Can't find project.clj!")))

(defun cljr-add-project-dependency (force)
  (interactive "P")
  (cljr--assert-leiningen-project)
  (cljr--assert-middleware)
  (-when-let* ((lib-name (->> (cljr--get-artifacts-from-middleware force)
                              (cljr--prompt-user-for "Artifact: ")))
               (version (->> (cljr--get-versions-from-middleware lib-name)
                             (cljr--prompt-user-for "Version: "))))
    (cljr--add-project-dependency lib-name version)))

(defun cljr--goto-fn-definition ()
  (if (or
       (re-search-backward "#("
                           (save-excursion (cljr--goto-toplevel) (point)) t)
       (re-search-backward "(fn \\["
                           (save-excursion (cljr--goto-toplevel) (point)) t))
      (when (looking-back "#")
        (backward-char))
    (error "Can't find definition of anonymous function!")))

(defun cljr--promote-fn ()
  (save-excursion
    (let ((fn (cljr--delete-and-extract-sexp))
          (name (read-string "Name: "))
          fn-start)
      (insert name)
      (cljr--goto-toplevel)
      (open-line 2)
      (insert fn)
      (paredit-backward-down)
      (cljr--goto-fn-definition)
      (setq fn-start (point))
      (forward-char)
      (insert "de")
      (paredit-forward)
      (insert " " name "\n")
      (re-search-forward "\\[")
      (paredit-forward-up)
      (unless (looking-at "\s*?$")
        (newline))
      (indent-region fn-start (cljr--point-after 'paredit-forward-up)))))

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
                      (save-excursion (paredit-forward-up 2) (point))))))

(defun cljr-promote-function (promote-to-defn)
  (interactive "P")
  (save-excursion
    (cljr--goto-fn-definition)
    (if (looking-at "#(")
        (cljr--promote-function-literal)
      (cljr--promote-fn)))
  (when current-prefix-arg
    (cljr--promote-fn)))

(defun cljr--populate-find-symbol-buffer (occurrence)
  (save-excursion
    (pop-to-buffer cljr--find-symbol-buffer)
    (end-of-buffer)
    (insert occurrence)))

(defun cljr--find-symbol-sync (symbol ns)
  (let* ((filename (buffer-file-name))
         (line (line-number-at-pos))
         (column (1+ (current-column)))
         (dir (file-truename
               (if cljr-find-symbols-in-dir-prompt
                   (read-directory-name "Base directory: " (cljr--project-dir))
                 (cljr--project-dir))))
         (find-symbol-request (list "op" "refactor"
                                    "ns" ns
                                    "clj-dir" dir
                                    "file" filename
                                    "loc-line" line
                                    "loc-column" column
                                    "refactor-fn" "find-symbol"
                                    "name" symbol)))
    (cljr--call-middleware-sync "occurrence" find-symbol-request)))

(defun cljr--find-symbol (force-project-dir symbol ns callback)
  (let* ((filename (buffer-file-name))
         (line (line-number-at-pos))
         (column (1+ (current-column)))
         (dir (file-truename
               (if (and (not force-project-dir) cljr-find-symbols-in-dir-prompt)
                   (read-directory-name "Base directory: " (cljr--project-dir))
                 (cljr--project-dir))))
         (find-symbol-request (list "op" "refactor"
                                    "ns" ns
                                    "clj-dir" dir
                                    "file" filename
                                    "loc-line" line
                                    "loc-column" column
                                    "refactor-fn" "find-symbol"
                                    "name" symbol)))
    (with-current-buffer (nrepl-current-connection-buffer)
      (setq cjr--occurrence-count 0)
      (setq cljr--num-syms -1))
    (cljr--call-middleware-async find-symbol-request callback)))

(defun cljr--format-and-insert-symbol-occurrence (occurrence-resp)
  (let ((occurrence (nrepl-dict-get occurrence-resp "occurrence"))
        (syms-count (nrepl-dict-get occurrence-resp "syms-count"))
        (cljr--find-symbol-buffer  "*cljr-find-usages*"))
    (when syms-count
      (setq cljr--num-syms syms-count))
    (when occurrence
      (setq cjr--occurrence-count (1+ cjr--occurrence-count)))
    (when occurrence
      (->> occurrence
           (apply (lambda (line _ col _ _ file match) (format "%s:%s: %s\n" file line match)))
           (cljr--populate-find-symbol-buffer)))
    (when (= cjr--occurrence-count cljr--num-syms)
      (cljr--finalise-find-symbol-buffer cljr--num-syms))))

(defun cljr--finalise-find-symbol-buffer (num-of-symbols)
  (with-current-buffer "*cljr-find-usages*"
    (insert (format "\nFind symbol finished: %d occurrence%s found"  num-of-symbols (if (> num-of-symbols 1) "s" "")))
    (grep-mode)))

(defun cljr--setup-find-symbol-buffer (symbol-name)
  (save-window-excursion
    (when (get-buffer cljr--find-symbol-buffer)
      (kill-buffer cljr--find-symbol-buffer))
    (pop-to-buffer cljr--find-symbol-buffer)
    (with-current-buffer "*cljr-find-usages*"
      (insert (format "-*- mode: grep; The symbol '%s' occurs in the following places:  -*-\n\n" symbol-name)))))

(defun cljr-find-usages ()
  (interactive)
  (cljr--assert-middleware)
  (save-buffer)
  (let* ((cljr--find-symbol-buffer "*cljr-find-usages*")
         (symbol (cider-symbol-at-point))
         (var-info (cider-var-info symbol))
         (ns (nrepl-dict-get var-info "ns"))
         (symbol-name (nrepl-dict-get var-info "name")))
    (cljr--setup-find-symbol-buffer (or symbol-name symbol))
    (cljr--find-symbol nil (or symbol-name symbol) ns 'cljr--format-and-insert-symbol-occurrence)))

(defun cljr--read-symbol-metadata (occurrences)
  (->> occurrences
       (-partition 7)
       (-map (lambda (symbol-meta)
               (apply (lambda (line-start line-end col-start col-end name file match)
                        (list :line-start line-start
                              :line-end line-end
                              :col-start col-start
                              :col-end col-end
                              :name (first (last (s-split "/" name)))
                              :file file
                              :match match))
                      symbol-meta)))))

(defun cljr--rename-symbol (ns occurrences new-name)
  (save-excursion
    (dolist (symbol-meta occurrences)
      (with-current-buffer
          (find-file-noselect (plist-get symbol-meta :file))
        (goto-char (point-min))
        (let* ((line-start (plist-get symbol-meta :line-start))
               (line-e (or (plist-get symbol-meta :line-end) line-start))
               (line-end line-start)
               (column-start (1- (or (plist-get symbol-meta :col-start) 1)))
               (start (progn (forward-line (1- line-start))
                             (move-to-column column-start)
                             (point)))
               (column-end (if (= line-start line-e)
                               (line-end-position)
                             (or (plist-get symbol-meta :col-end) (line-end-position))))
               (end (progn (goto-char (point-min))
                           (forward-line (1- line-e))
                           (move-to-column column-end)
                           (point)))
               (orig-name (plist-get symbol-meta :name))
               (name (regexp-quote orig-name))
               (matches-count 0)
               (replaced nil))
          (goto-char start)
          (while (re-search-forward name end t)
            (setq matches-count (1+ matches-count)))
          (goto-char start)
          (if (and (= 1 matches-count) (re-search-forward name end t))
              (replace-match new-name)
            (while (and (not replaced) (re-search-forward name end t))
              (let* ((original-point (point))
                     (cider-symbol (cider-symbol-at-point))
                     (var-info (cider-var-info cider-symbol))
                     (symbol-ns (nrepl-dict-get var-info "ns"))
                     (symbol-name (nrepl-dict-get var-info "name"))
                     (word-start (progn (forward-word -1)
                                        (point))))
                (when (and (string= symbol-ns ns) (string= symbol-name orig-name))
                  (perform-replace name new-name nil nil nil nil nil word-start original-point)
                  (setq replaced t))
                (goto-char original-point)))))
        (save-buffer)))))

(defun cljr-rename-symbol (new-name)
  (interactive "sRename to: ")
  (cljr--assert-middleware)
  (save-buffer)
  (let* ((symbol (cider-symbol-at-point))
         (var-info (cider-var-info symbol))
         (symbol-name (nrepl-dict-get var-info "name"))
         (ns (nrepl-dict-get var-info "ns")))
    (let* ((occurrences (-> (or symbol-name symbol)
                            (cljr--find-symbol-sync ns)
                            cljr--read-symbol-metadata))
           (buffer-of-symbol (cider-find-var-file (concat ns "/" symbol-name)))
           (tooling-buffer-p (cider--tooling-file-p (buffer-name buffer-of-symbol))))
      (cljr--rename-symbol ns occurrences new-name)
      (when (and (not tooling-buffer-p) symbol-name)
        (message "Reloading buffer %s where symbol is defined" buffer-of-symbol))
      (message "Renamed %s occurrences of %s" (length occurrences) (or symbol-name symbol))
      (when (not tooling-buffer-p)
        (when symbol-name
          (cider-load-buffer buffer-of-symbol))
        (cljr-warm-ast-cache)))))

(defun cljr-warm-ast-cache ()
  (interactive)
  (cljr--find-symbol t "join" "clojure.string" (lambda (_))))

(defun cljr--replace-ns (new-ns)
  (save-excursion
    (cljr--goto-ns)
    (cljr--delete-and-extract-sexp)
    (insert new-ns)
    (cljr--goto-ns)
    (indent-region (point) (cljr--point-after 'paredit-forward))
    (paredit-forward)
    (cljr--just-one-blank-line)))

(defun cljr-clean-ns ()
  (interactive)
  (cljr--assert-leiningen-project)
  (cljr--assert-middleware)
  (save-buffer)
  (let ((result (nrepl-send-sync-request
                 (list "op" "clean-ns"
                       "path" (buffer-file-name)))))
    (-when-let (error-msg (nrepl-dict-get result "error"))
      (error error-msg))
    (-when-let (new-ns (nrepl-dict-get result "ns"))
      (cljr--replace-ns new-ns))))

(defun cljr--narrow-candidates (candidates)
  (cond ((= (length candidates) 0)
         (error "Couldn't find any symbols matching %s on classpath."
                (cljr--normalize-symbol-name symbol)))
        ((= (length candidates) 1)
         (first candidates))
        (t
         (cljr--prompt-user-for "Require: " candidates))))

(defun cljr--insert-libspec-verbosely (libspec)
  (insert libspec)
  (message "%s added to ns" libspec))

(defun cljr--insert-missing-import (missing)
  (save-excursion
    (cljr--insert-in-ns ":import")
    (cljr--insert-libspec-verbosely missing)))

(defun cljr--insert-missing-require (symbol missing)
  (save-excursion
    (cljr--insert-in-ns ":require")
    (let ((alias? (s-contains? "/" symbol)))
      (if alias?
          (let ((prefix (first (s-split "/" symbol))))
            (cljr--insert-libspec-verbosely (format "[%s :as %s]" missing prefix)))
        (cljr--insert-libspec-verbosely (format "[%s :refer [%s]]"
                                                missing symbol))))))

(defun cljr--add-missing-libspec (symbol candidates type)
  (let* ((candidates (and candidates (s-split " " candidates)))
         (missing (cljr--narrow-candidates candidates)))
    (if (string= type "require")
        (cljr--insert-missing-require symbol missing)
      (cljr--insert-missing-import missing))))

(defun cljr--normalize-symbol-name (name)
  "Removes prefix and reader macros

java.util.Date. -> Date
str/split -> split
Date. -> Date"
  (cond ((s-ends-with? "." name)
         (->> name (s-chop-suffix ".") cljr--normalize-symbol-name))
        ((s-contains? "/" name) (->> name (s-split "/") second))
        ((s-matches? "\\w+\\.\\w+" name)
         (->> name (s-split "\\.") last car))
        (t name)))

(defun cljr--call-middleware-to-resolve-missing (symbol)
  ;; Just so this part can be mocked out in a step definition
  (nrepl-send-sync-request
   (list "op" "resolve-missing"
         "symbol" (cljr--normalize-symbol-name symbol))))

(defun cljr--maybe-rethrow-error (response)
  (-when-let (err (nrepl-dict-get response "error"))
    (error err)))

(defun cljr-add-missing-libspec ()
  "Requires or imports the symbol at point.

If the symbol at point is of the form str/join then the ns
containing join will be aliased to str."
  (interactive)
  (cljr--assert-middleware)
  (let* ((symbol (cider-symbol-at-point))
         (response (cljr--call-middleware-to-resolve-missing symbol))
         (type (nrepl-dict-get response "type"))
         (candidates (nrepl-dict-get response "candidates")))
    (cljr--maybe-rethrow-error response)
    (cljr--add-missing-libspec symbol candidates type)))

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

(defun cljr-hotload-dependency ()
  "Download a dependency (if needed) and hotload it into the current repl session.

Defaults to the dependency vector at point, but prompts if none is found."
  (interactive)
  (cljr--assert-middleware)
  (-> (or (cljr--dependency-vector-at-point)
          (cljr--prompt-user-for "Dependency vector: "))
      cljr--assert-dependency-vector
      cljr--call-middleware-to-hotload-dependency))

(defun cljr--insert-function (name body public?)
  (save-excursion
    (cljr--goto-toplevel)
    (open-line 2)
    (if public?
        (insert "(defn ")
      (insert "(defn- "))
    (insert name)
    (newline-and-indent)
    (insert "[]")
    (newline-and-indent)
    (insert body ")")))

(defun cljr--call-middleware-to-find-unbound-vars (ns)
  (let ((response (nrepl-send-sync-request
                   (list "op" "find-unbound" "ns" ns))))
    (cljr--maybe-rethrow-error response)
    (nrepl-dict-get response "unbound")))

(defun cljr--goto-enclosing-sexp ()
  (let ((sexp-regexp (rx (or "(" "#{" "{" "["))))
    (unless (looking-at sexp-regexp)
      (paredit-backward-up))
    (when (looking-back "#")
      (forward-char -1))))

(defun cljr-extract-function ()
  "Extract the form at point, or the nearest enclosing form, into
  a toplevel defn.

With a prefix the newly created defn will be public."
  (interactive)
  (cljr--assert-middleware)
  (cljr--goto-enclosing-sexp)
  (let* ((body (cljr--delete-and-extract-sexp))
         (public? current-prefix-arg)
         (placeholder "#a015f65")
         (name (cljr--prompt-user-for "Name: "))
         (fn-regexp (s-concat "(defn-? " name))
         unbound irrelevant-buffer-content)
    (insert "(" name " " placeholder)
    (cljr--insert-function name body public?)

    ;; Delete any code following the newly created fn before calling middleware
    ;; as that part of the buffer might be in a bad state.
    (re-search-backward fn-regexp)
    (paredit-forward)
    (setq irrelevant-buffer-content (cljr--extract-region (point) (point-max)))
    (save-buffer)
    (setq unbound (cljr--call-middleware-to-find-unbound-vars
                   (cljr--current-namespace)))
    ;; Insert args in new fn
    (re-search-backward fn-regexp)
    (paredit-forward-down 2)
    (insert unbound)
    ;; restore end of buffer
    (paredit-forward-up 2)
    (insert irrelevant-buffer-content)
    ;; Insert args at call-site
    (re-search-backward placeholder)
    (delete-region (point) (cljr--point-after 'paredit-forward))
    (insert unbound)
    (when (and (s-blank? unbound) (looking-back " "))
      (backward-delete-char 1))

    (re-search-backward fn-regexp)
    (indent-region (point) (point-max))
    (re-search-forward (s-concat "\(" name unbound))))

;; ------ minor mode -----------
;;;###autoload
(define-minor-mode clj-refactor-mode
  "A mode to keep the clj-refactor keybindings."
  nil " cljr" clj-refactor-map)

(provide 'clj-refactor)
;;; clj-refactor.el ends here
