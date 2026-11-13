;;; miscellany-test.el --- Specs for the paredit commands clj-refactor wraps -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-raise-sexp"
  (it "takes the place of paredit's command and drops the # of a function literal"
    (expect (cljr-test-refactor "(map #(partial |f) l)"
              (paredit-mode 1)
              (cljr-test-keys "M-r"))
            :to-equal "(map f l)"))

  (it "drops the # of a set literal"
    (expect (cljr-test-refactor "(map #{|f} l)"
              (cljr-raise-sexp))
            :to-equal "(map f l)"))

  (it "raises as many forms as the prefix argument says"
    (expect (cljr-test-refactor "(map #(partial |f g) l)"
              (cljr-raise-sexp 2))
            :to-equal "(map f g l)"))

  (it "raises a form out of a plain list like paredit does"
    (expect (cljr-test-refactor "(map (partial |f) l)"
              (cljr-raise-sexp))
            :to-equal "(map f l)"))

  (it "leaves point before the raised form"
    (expect (cljr-test-with-buffer "(map #(partial |f) l)"
              (cljr-raise-sexp)
              (cljr-test-buffer-string t))
            :to-equal "(map |f l)")))

(describe "cljr-splice-sexp-killing-backward"
  (it "takes the place of paredit's command and drops the # of a function literal"
    (expect (cljr-test-refactor "(map #(partial |f) l)"
              (paredit-mode 1)
              (cljr-test-keys "M-<up>"))
            :to-equal "(map f l)"))

  (it "keeps what follows point in the literal"
    (expect (cljr-test-with-buffer "(map #(partial |f d) l)"
              (cljr-splice-sexp-killing-backward)
              (cljr-test-buffer-string t))
            :to-equal "(map |f d l)"))

  (it "splices a plain list like paredit does"
    (expect (cljr-test-refactor "(map (partial |f) l)"
              (cljr-splice-sexp-killing-backward))
            :to-equal "(map f l)")))

(describe "cljr-splice-sexp-killing-forward"
  (it "takes the place of paredit's command and drops the # of a function literal"
    (expect (cljr-test-refactor "(map #(partial f| d) l)"
              (paredit-mode 1)
              (cljr-test-keys "M-<down>"))
            :to-equal "(map partial f l)"))

  (it "leaves point where the literal was cut"
    (expect (cljr-test-with-buffer "(map #(partial f| d) l)"
              (cljr-splice-sexp-killing-forward)
              (cljr-test-buffer-string t))
            :to-equal "(map partial f| l)"))

  (it "splices a plain list like paredit does"
    (expect (cljr-test-refactor "(map (partial f| d) l)"
              (cljr-splice-sexp-killing-forward))
            :to-equal "(map partial f l)")))

;;; miscellany-test.el ends here
