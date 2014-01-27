;;; clj-refactor.el --- A collection of clojure refactoring functions

;; Copyright © 2012 Magnar Sveen <magnars@gmail.com>

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.9.0
;; Keywords: convenience
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (yasnippet "0.6.1") (paredit "22") (multiple-cursors "1.2.2"))

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

;; ## Installation
;;
;; I highly recommended installing clj-refactor through elpa.
;;
;; It's available on [marmalade](http://marmalade-repo.org/) and
;; [melpa](http://melpa.milkbox.net/):
;;
;;     M-x package-install clj-refactor
;;
;; You can also install the dependencies on your own, and just dump
;; clj-refactor in your path somewhere:
;;
;;  - <a href="https://github.com/magnars/s.el">s.el</a>
;;  - <a href="https://github.com/magnars/dash.el">dash.el</a>
;;

;; ## Setup
;;
;;     (require 'clj-refactor)
;;     (add-hook 'clojure-mode-hook (lambda ()
;;                                    (clj-refactor-mode 1)
;;                                    ;; insert keybinding setup here
;;                                    ))
;;
;; You'll also have to set up the keybindings in the lambda. Read on.

;; ## Setup keybindings
;;
;; All functions in clj-refactor have a two-letter mnemonic shortcut. You
;; get to choose how those are bound. Here's how:
;;
;;     (cljr-add-keybindings-with-prefix "C-c C-m")
;;     ;; eg. rename files with `C-c C-m rf`.
;;
;; If you would rather have a modifier key, instead of a prefix, do:
;;
;;     (cljr-add-keybindings-with-modifier "C-s-")
;;     ;; eg. rename files with `C-s-r C-s-f`.
;;
;; If neither of these appeal to your sense of keyboard layout aesthetics, feel free
;; to pick and choose your own keybindings with a smattering of:
;;
;;     (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

;; ## Use
;;
;; This is it so far:
;;
;;  - `rf`: rename file, update ns-declaration, and then query-replace new ns in project.
;;  - `ar`: add :require to namespace declaration, then jump back
;;  - `au`: add :use to namespace declaration, then jump back
;;  - `ai`: add :import to namespace declaration, then jump back
;;  - `th`: thread another expression
;;  - `uw`: unwind a threaded expression
;;
;; Combine with your keybinding prefix/modifier.

;; ## Automatic insertion of namespace declaration
;;
;; When you open a blank `.clj`-file, clj-refactor inserts the namespace
;; declaration for you.
;;
;; It will also add the relevant `:use` clauses in test files, normally
;; using `clojure.test`, but if you're depending on midje in your
;; `project.clj` it uses that instead.
;;
;; Like clojure-mode, clj-refactor presumes that you are postfixing your
;; test files with `_test`.
;;
;; Prefer to insert your own ns-declarations? Then:
;;
;; (setq clj-add-ns-to-blank-clj-files nil)

;;; Code:

(require 'dash)
(require 's)
(require 'yasnippet)
(require 'paredit)
(require 'multiple-cursors)

(defcustom cljr-add-ns-to-blank-clj-files t
  "When true, automatically adds a ns form to new clj files."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-auto-sort-ns t
  "When true, sort ns form whenever adding to the form using clj-refactor
   functions."
  :group 'cljr
  :type 'boolean)

(defcustom cljr-use-metadata-for-privacy nil
  "When nil, `cljr-cycle-privacy` will use (defn- f []).
   When t, it will use (defn ^:private f [])"
  :group 'cljr
  :type 'boolean)

(defvar clj-refactor-map (make-sparse-keymap) "")

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
  (define-key clj-refactor-map (funcall key-fn "rf") 'cljr-rename-file)
  (define-key clj-refactor-map (funcall key-fn "ru") 'cljr-replace-use)
  (define-key clj-refactor-map (funcall key-fn "au") 'cljr-add-use-to-ns)
  (define-key clj-refactor-map (funcall key-fn "ar") 'cljr-add-require-to-ns)
  (define-key clj-refactor-map (funcall key-fn "ai") 'cljr-add-import-to-ns)
  (define-key clj-refactor-map (funcall key-fn "sn") 'cljr-sort-ns)
  (define-key clj-refactor-map (funcall key-fn "th") 'cljr-thread)
  (define-key clj-refactor-map (funcall key-fn "uw") 'cljr-unwind)
  (define-key clj-refactor-map (funcall key-fn "ua") 'cljr-unwind-all)
  (define-key clj-refactor-map (funcall key-fn "il") 'cljr-introduce-let)
  (define-key clj-refactor-map (funcall key-fn "el") 'cljr-expand-let)
  (define-key clj-refactor-map (funcall key-fn "ml") 'cljr-move-to-let)
  (define-key clj-refactor-map (funcall key-fn "tf") 'cljr-thread-first-all)
  (define-key clj-refactor-map (funcall key-fn "tl") 'cljr-thread-last-all)
  (define-key clj-refactor-map (funcall key-fn "cp") 'cljr-cycle-privacy)
  (define-key clj-refactor-map (funcall key-fn "cc") 'cljr-cycle-coll)
  (define-key clj-refactor-map (funcall key-fn "cs") 'cljr-cycle-stringlike)
  (define-key clj-refactor-map (funcall key-fn "ad") 'cljr-add-declaration))

;;;###autoload
(defun cljr-add-keybindings-with-prefix (prefix)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-prefix prefix)))

;;;###autoload
(defun cljr-add-keybindings-with-modifier (modifier)
  (cljr--add-keybindings (-partial 'cljr--key-pairs-with-modifier modifier)))

;; ------ utilities -----------

(defun cljr--delete-and-extract-sexp ()
  (let* ((beg (point))
         (end (progn (paredit-forward)
                     (point)))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    contents))

(defun cljr--goto-toplevel ()
  (let ((depth (first (paredit-current-parse-state))))
    (paredit-backward-up depth)))

;; ------ file -----------

(defun cljr--project-dir ()
  (file-truename
   (locate-dominating-file default-directory "project.clj")))

(defun cljr--project-file ()
  (expand-file-name "project.clj" (cljr--project-dir)))

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
  (let ((bound (save-excursion (forward-list 1) (point))))
    (if (search-forward (concat "(" type) bound t)
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
      (forward-char -1))))

(defun cljr--project-depends-on (package)
  (save-window-excursion
    (find-file (cljr--project-file))
    (goto-char (point-min))
    (search-forward package nil t)))

(defun cljr--add-test-use-declarations ()
  (save-excursion
    (let ((ns (clojure-find-ns)))
      (cljr--insert-in-ns ":require")
      (insert "[" (s-chop-suffix "-test" ns) " :refer :all]")
      (cljr--insert-in-ns ":require")
      (insert "[" (if (cljr--project-depends-on "midje")
                      "midje.sweet"
                    "clojure.test")
              " :refer :all]"))))

(defun cljr--add-ns-if-blank-clj-file ()
  (ignore-errors
    (when (and cljr-add-ns-to-blank-clj-files
               (s-ends-with? ".clj" (buffer-file-name))
               (= (point-min) (point-max)))
      (clojure-insert-ns-form)
      (newline 2)
      (when (clojure-in-tests-p)
        (cljr--add-test-use-declarations)))))

(add-hook 'find-file-hook 'cljr--add-ns-if-blank-clj-file)

(defun cljr--extract-ns-statements (statement-type)
  (cljr--goto-ns)
  (let ((bound (save-excursion (forward-list 1) (point))))
    (if (not (re-search-forward (concat "(" statement-type) bound t))
        '()
      (let (statements)
        (while (not (looking-at " *)"))
          (push (cljr--delete-and-extract-sexp) statements))
        statements))))

(defun cljr--only-alpha-chars (s)
  (replace-regexp-in-string "[^A-Za-z]" "" s))

;;;###autoload
(defun cljr-sort-ns ()
  (interactive)
  (save-excursion
    (dolist (statement-type '(":require" ":use" ":import"))
      (ignore-errors
        (dolist (statement (->> (cljr--extract-ns-statements statement-type)
                             (-map 's-trim)
                             (-sort (lambda (s1 s2)
                                      (string< (cljr--only-alpha-chars s1)
                                               (cljr--only-alpha-chars s2))))
                             (-distinct)))
          (cljr--insert-in-ns statement-type)
          (insert statement))))))

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
  (yas/expand-snippet "${1:[${2:$3 :as $4}]}$0"))

;;;###autoload
(defun cljr-add-use-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":require")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas/expand-snippet "[$1 :refer ${2::all}]$0"))

;;;###autoload
(defun cljr-add-import-to-ns ()
  (interactive)
  (set-marker cljr--tmp-marker (point))
  (cljr--insert-in-ns ":import")
  (cljr--pop-tmp-marker-after-yasnippet)
  (when cljr-auto-sort-ns
    (cljr--add-yas-snippet-sort-ns-hook))
  (yas/expand-snippet "$1"))

(defun cljr--extract-ns-from-use ()
  (let ((form (format "%s" (cljr--delete-and-extract-sexp))))
    (substring form 1 (1- (length form)))))

(defun cljr--extract-multiple-ns-from-use ()
  (let* ((form (format "%s" (cljr--delete-and-extract-sexp)))
         (form (substring form 1 (1- (length form))))
         (words (s-split " " form))
         (prefix (pop words))
         (libs (nreverse words)))
    (-map (lambda (lib) (concat prefix "." lib)) libs)))

(defun cljr--multiple-namespaces-p (use-form)
  "Returns t if the use form looks like [some.lib ns1 ns2 ...]"
  (s-matches-p "[[A-z0-9.]+ \\(\\([A-z0-9]+ \\)\\|\\([A-z0-9]+\\)\\)+]"
               (format "%s" use-form)))

(defun cljr--more-namespaces-in-use-p (use-start)
  (goto-char use-start)
  (let ((use-end (save-excursion (forward-sexp) (point)) ))
    (prog1
        (re-search-forward "[.*]" use-end t)
      (paredit-backward-up))))

(defun cljr--extract-used-namespaces ()
  (let (libs use-start use-end)
    (cljr--goto-ns)
    (if (not (re-search-forward "(:use " nil t))
        (message "There is no :use clause in the ns declaration.")
      (save-excursion
        (paredit-backward-up)
        (setq use-start (point))
        (paredit-forward)
        (setq use-end (1- (point))))
      (while (cljr--more-namespaces-in-use-p use-start)
        (push (if (cljr--multiple-namespaces-p (sexp-at-point))
                  (cljr--extract-multiple-ns-from-use)
                (cljr--extract-ns-from-use))
              libs))
      (goto-char use-start)
      (cljr--delete-and-extract-sexp)
      (join-line)
      (delete-char 1)
      (nreverse (-flatten libs)))))

;;;###autoload
(defun cljr-replace-use ()
  (interactive)
  (save-excursion
    (dolist (used-ns (cljr--extract-used-namespaces))
      (cljr--insert-in-ns ":require")
      (insert (format "[%s :refer :all]" used-ns)))))

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
          (end (progn (paredit-forward) (point))))
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

;;;###autoload
(defun cljr-unwind ()
  (interactive)
  (ignore-errors
    (forward-char 3))
  (search-backward-regexp "\\((some->\\)\\|\\((->\\)")
  (if (cljr--nothing-more-to-unwind)
      (progn
        (cljr--pop-out-of-threading)
        nil)
    (paredit-forward-down)
    (cond
     ((looking-at "->[\n\r\t ]")     (cljr--unwind-first))
     ((looking-at "some->[\n\r\t ]") (cljr--unwind-first))
     ((looking-at "->>[\n\r\t ]")     (cljr--unwind-last))
     ((looking-at "some->>[\n\r\t ]") (cljr--unwind-last)))
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
        (progn
          (message "Nothing more to thread.")
          nil)
      (delete-region beg end)
      (paredit-backward-up)
      (just-one-space 0)
      (insert contents)
      (newline-and-indent)
      (cljr--remove-superfluous-parens)
      t)))

(defun cljr--thread-last ()
  (paredit-forward)
  (paredit-forward)
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
  (ignore-errors
    (forward-char 7))
  (search-backward-regexp "\\((some->\\)\\|\\((->\\)")
  (paredit-forward-down)
  (if (not (cljr--thread-guard))
      nil
    (cond
     ((looking-at "->[\n\r\t ]")     (cljr--thread-first))
     ((looking-at "some->[\n\r\t ]") (cljr--thread-first))
     ((looking-at "->>[\n\r\t ]")     (cljr--thread-last))
     ((looking-at "some->>[\n\r\t ]") (cljr--thread-last)))))

;;;###autoload
(defun cljr-thread-first-all ()
  (interactive)
  (paredit-wrap-round)
  (insert "-> ")
  (while (cljr-thread)
    t))

;;;###autoload
(defun cljr-thread-last-all ()
  (interactive)
  (paredit-wrap-round)
  (insert "->> ")
  (while (cljr-thread)
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

(defun cljr--go-to-let ()
  (search-backward-regexp "\(\\(when-let\\|if-let\\|let\\)\\( \\|\\[\\)"))

;;;###autoload
(defun cljr-expand-let ()
  (interactive)
  (ignore-errors
    (forward-char 4))
  (cljr--go-to-let)
  (paredit-forward-down 2)
  (paredit-forward-up)
  (skip-syntax-forward " >")
  (paredit-convolute-sexp))

;;;###autoload
(defun cljr-move-to-let ()
  (interactive)
  (save-excursion
    (let ((contents (cljr--delete-and-extract-sexp)))
      (cljr--go-to-let)
      (search-forward "[")
      (paredit-backward)
      (paredit-forward)
      (paredit-backward-down)
      (backward-char)
      (if (looking-at "\\[ *\\]")
          (forward-char)
        (forward-char)
        (newline-and-indent))
      (insert contents))
    (backward-sexp)
    (insert " ")
    (backward-char)
    (mc/create-fake-cursor-at-point))
  (mc/maybe-multiple-cursors-mode))

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
  "convert the string or keyword at (point) from string -> keyword or keyword -> string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "_w")
    (cond
     ((eq ?\" (char-before))
      (backward-char)
      (insert ":" (substring (cljr--delete-and-extract-sexp) 1 -1)))
     ((looking-at "\"")
      (insert ":" (substring (cljr--delete-and-extract-sexp) 1 -1)))
     ((looking-at ":")
      (insert "\"" (substring (cljr--delete-and-extract-sexp) 1) "\""))
     (t
      (message "Couldn't cljr-cycle-stringlike")))))

;;;###autoload
(defun cljr-cycle-coll ()
  "convert the coll at (point) from (x) -> {x} -> [x] -> -> #{x} -> (x) recur"
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

;; ------ minor mode -----------

;;;###autoload
(define-minor-mode clj-refactor-mode
  "A mode to keep the clj-refactor keybindings."
  nil " cljr" clj-refactor-map)

(provide 'clj-refactor)
;;; clj-refactor.el ends here
