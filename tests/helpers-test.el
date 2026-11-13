;;; helpers-test.el --- Specs for the helpers of the specs -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-ask-twice ()
  "Ask two questions; return the answers."
  (interactive)
  (setq cljr-test-answers (list (read-string "First: ")
                                (read-string "Second: " "default"))))

(defvar cljr-test-answers nil
  "What `cljr-test-ask-twice' was told.")

(describe "cljr-test-with-buffer"
  (it "puts point where the | is"
    (cljr-test-with-buffer "(foo |bar)"
      (expect (cljr-test-buffer-string) :to-equal "(foo bar)")
      (expect (looking-at-p "bar") :to-be-truthy)
      (expect (cljr-test-buffer-string t) :to-equal "(foo |bar)")))

  (it "puts point at the end when there's no |"
    (cljr-test-with-buffer "(foo bar)"
      (expect (eobp) :to-be-truthy)))

  (it "sets the buffer up the way a user's is"
    (cljr-test-with-buffer ""
      (expect major-mode :to-be 'clojure-mode)
      (expect clj-refactor-mode :to-be-truthy)
      (expect yas-minor-mode :to-be-truthy)))

  (it "leaves no buffer behind"
    (let ((buffers (buffer-list)))
      (cljr-test-with-buffer "(foo)")
      (expect (buffer-list) :to-equal buffers))))

(describe "cljr-test-call"
  (it "answers the prompts of the command in order"
    (cljr-test-with-buffer ""
      (cljr-test-call #'cljr-test-ask-twice "one" "two")
      (expect cljr-test-answers :to-equal '("one" "defaulttwo"))))

  (it "fails when a prompt is left without an answer"
    (cljr-test-with-buffer ""
      (expect (cljr-test-call #'cljr-test-ask-twice "one")
              :to-throw 'error '("Nothing left to answer the prompt with: Second: ")))))

(describe "cljr-test-keys"
  (it "fails when a yes-or-no question is left without an answer"
    (cljr-test-with-buffer ""
      (local-set-key (kbd "<f6>") (lambda () (interactive) (y-or-n-p "Sure? ")))
      (expect (cljr-test-keys "<f6>")
              :to-throw 'error '("Nothing left to answer the prompt with: Sure? ")))))

(describe "cljr-test-with-project"
  (it "sets up a project that's gone afterwards"
    (let (root)
      (cljr-test-with-project (:name "cljr"
                               :files '(("src/cljr/core.clj" . "(ns cljr.core)\n")))
        (setq root cljr-test-project-root)
        (expect (file-exists-p (cljr-test-file "project.clj")) :to-be-truthy)
        (expect (cljr-test-file-string "src/cljr/core.clj") :to-equal "(ns cljr.core)\n")
        (cljr-test-visit "src/cljr/core.clj" "(ns cljr.core)\n\n(def |x 1)\n")
        (expect (looking-at-p "x 1") :to-be-truthy)
        (expect (cljr-test-file-string "src/cljr/core.clj")
                :to-equal "(ns cljr.core)\n\n(def x 1)\n")
        (expect (cljr-test-file-string "src/cljr/core.clj" 'on-disk)
                :to-equal "(ns cljr.core)\n"))
      (expect (file-exists-p root) :to-be nil)
      (expect (cl-some (lambda (buffer)
                         (let ((file (buffer-file-name buffer)))
                           (and file (string-prefix-p root file))))
                       (buffer-list))
              :to-be nil))))

;;; helpers-test.el ends here
