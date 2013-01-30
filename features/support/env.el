;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq clj-refactor-root-path project-directory))

(add-to-list 'load-path clj-refactor-root-path)

(require 'clojure-mode)
(require 'clj-refactor)
(require 'espuds)
(require 'ert)
(require 's)

(defun clojure-expected-ns ()
  "Returns the namespace name that the file should have."
  (let* ((project-dir (file-truename
                       (locate-dominating-file default-directory
                                               "project.clj")))
         (relative (substring (file-truename (buffer-file-name)) (length project-dir) -4)))
    (replace-regexp-in-string
     "_" "-" (mapconcat 'identity (cdr (split-string relative "/")) "."))))

(cljr-add-keybindings-with-prefix "C-!")

(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode)))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
