(require 'paredit)
(require 'clj-refactor)
(require 'buttercup)

;; NOTE: please remember, without an `it` block, your tests will not be run!
;; You can learn about Buttercup's syntax here:
;; https://github.com/jorgenschaefer/emacs-buttercup/blob/v1.24/docs/running-tests.md

(describe "cljr--ns-name"
  (it "returns the ns name of its argument"
    (expect (cljr--ns-name "com.corp.foo") :to-equal "foo")
    (expect (cljr--ns-name "foo") :to-equal "foo")))

(defmacro cljr--with-clojure-temp-file (filename &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local buffer-file-name ,filename)
     (delay-mode-hooks (clojure-mode))
     ,@body))

(describe "cljr--insert-require-libspec"
  :var (after-inserting-a
        after-inserting-b)
  (before-each
    (cljr--with-clojure-temp-file "libspec.clj"
      (insert "(ns lib)\n")
      (cljr--insert-require-libspec "[com.a :as a]")
      (setq after-inserting-a (buffer-string))
      (cljr--insert-require-libspec "[com.b :as b]")
      (setq after-inserting-b (buffer-string))))

  (it "adds a new require over an empty ns form"
    (expect after-inserting-a :to-equal
            "(ns lib\n  (:require [com.a :as a]))\n"))
  (it "adds a new require over a ns with existing requires"
    (expect after-inserting-b :to-equal
            "(ns lib
  (:require [com.a :as a]
            [com.b :as b]))
")))

(describe "cljr--ns-alias-at-point"
  (it "returns the short alias before the /"
    (expect (with-temp-buffer
              (insert "aba/")
              (cljr--ns-alias-at-point))
            :to-equal "aba"))

  (it "removes namespace keyword ::"
    (expect (with-temp-buffer
              (insert "::ns-name/")
              (cljr--ns-alias-at-point))
            :to-equal "ns-name"))

  (it "removes deref operator"
    (expect (with-temp-buffer
              (insert "@atom/")
              (cljr--ns-alias-at-point))
            :to-equal "atom")))

(describe "cljr--unresolved-alias-ref"
  (it "returns unresolved alias reference"
    (expect (cljr--with-clojure-temp-file "foo.clj"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "ns-name"))
            :to-equal "ns-name"))

  (it "returns nil for resolved alias"
    (expect (cljr--with-clojure-temp-file "foo.clj"
              (insert "(ns foo (:require [user.ns-name :as ns-name]))")
              (cljr--unresolved-alias-ref "ns-name"))
            :to-be nil))

  (it "returns nil for js alias in cljs file."
    (expect (cljr--with-clojure-temp-file "foo.cljs"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "js"))
            :to-be nil))

  (it "returns js for js alias in clj file."
    (expect (cljr--with-clojure-temp-file "foo.clj"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "js"))
            :to-equal "js"))

  (it "returns js for js alias in cljc file."
    (expect (cljr--with-clojure-temp-file "foo.cljc"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "js"))
            :to-equal "js")))
