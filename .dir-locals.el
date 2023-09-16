;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (bug-reference-url-format . "https://github.com/clojure-emacs/clj-refactor.el/issues/%s")
  (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (sentence-end-double-space . t)
  (emacs-lisp-docstring-fill-column . 75)
  (checkdoc-symbol-words . ("top-level" "major-mode" "macroexpand-all" "print-level" "print-length"))
  (checkdoc-package-keywords-flag)
  (byte-compile-docstring-max-column 240)
  (checkdoc-force-docstrings-flag nil)
  (checkdoc-arguments-in-order-flag)
  (checkdoc-verb-check-experimental-flag)
  (elisp-lint-indent-specs . ((if-let* . 2)
                              (when-let* . 1)
                              (let* . defun)
                              (nrepl-dbind-response . 2)
                              ;; need better solution for indenting cl-flet bindings
                              (insert-label . defun)              ;; cl-flet
                              (insert-align-label . defun)        ;; cl-flet
                              (insert-rect . defun)               ;; cl-flet
                              (cl-defun . 2)
                              (cljr--update-file . 1)
                              (cljr--with-string-content . 1)
                              (with-parsed-tramp-file-name . 2)
                              (thread-first . 0)
                              (thread-last . 0)
                              (transient-define-prefix . defmacro)
                              (transient-define-suffix . defmacro)))))

;; To use the bug-reference stuff, do:
;;     (add-hook 'text-mode-hook #'bug-reference-mode)
;;     (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
