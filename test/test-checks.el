;; This is a script to be loaded from the root `clj-refactor' directory. It will
;; prepare all requirements and then run `check-declare-directory' on
;; `default-directory'. For example: emacs -Q --batch -l test/test-checkdoc.el

;; This assumes that all `clj-refactor' dependencies are already on the package
;; dir (probably from running `cask install').

(add-to-list 'load-path (expand-file-name "./"))
(require 'package)
(require 'check-declare)
(package-initialize)

;; disable some annoying (or non-applicable) checkdoc checks

(let ((files (directory-files default-directory t
                              "\\`[^.].*\\.el\\'" t)))

  ;; `checkdoc-file' was introduced in Emacs 25
  

  (when (apply #'check-declare-files files)
    (kill-emacs 1)))
