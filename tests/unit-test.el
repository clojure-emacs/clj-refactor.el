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

(defun cljr--alias-here (content)
  (with-temp-buffer
    (clojure-mode) ;; activate clojure syntax table
    (insert content)
    (cljr--ns-alias-at-point)))

;; https://clojure.org/reference/reader#_reader_forms
(describe "cljr--ns-alias-at-point"
  (it "returns the short alias before the /"
    (expect (cljr--alias-here "aba/")
            :to-equal "aba"))

  (it "does not include boundaries"
    (expect (cljr--alias-here " alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "(alias/")
            :to-equal "alias")
    (expect (cljr--alias-here ",alias/")
            :to-equal "alias"))

  (it "identifies dotted namespace aliases"
    (expect (cljr--alias-here "a.b.c/")
            :to-equal "a.b.c"))

  (it "removes prefix :: (namespaced keyword)"
    (expect (cljr--alias-here "::ns-name/")
            :to-equal "ns-name")
    (expect (cljr--alias-here "(::util/")
            :to-equal "util"))

  (it "includes question-mark"
    (expect (cljr--alias-here "ns-name?/")
            :to-equal "ns-name?")
    (expect (cljr--alias-here "ns?name/")
            :to-equal "ns?name")
    (expect (cljr--alias-here "?ns-name/")
            :to-equal "?ns-name"))

  (it "removes prefix : (keyword)"
    (expect (cljr--alias-here ":ns-name/")
            :to-equal "ns-name")
    (expect (cljr--alias-here "(:alias/")
            :to-equal "alias"))

  (it "allows infix :"
    (expect (cljr--alias-here "foo:bar/")
            :to-equal "foo:bar"))

  (it "allows infix $"
    (expect (cljr--alias-here "foo$bar/")
            :to-equal "foo$bar"))

  (it "removes deref operator"
    (expect (cljr--alias-here "@atom/")
            :to-equal "atom"))

  (it "removes prefix quote"
    (expect (cljr--alias-here "'name-bar.set/")
            :to-equal "name-bar.set"))

  (it "allows infix quote"
    (expect (cljr--alias-here "tl'an/")
            :to-equal "tl'an"))

  (it "removes sharpquote"
    (expect (cljr--alias-here "#'alias/")
            :to-equal "alias"))

  (it "removes quasiquote"
    (expect (cljr--alias-here "`alias/")
            :to-equal "alias"))

  (it "ignores prefix digits"
    (expect (cljr--alias-here "0alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "01alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "{01alias/")
            :to-equal "alias"))

  (it "allows internal & suffix digits"
    (expect (cljr--alias-here "alias0/")
            :to-equal "alias0")
    (expect (cljr--alias-here "ali0as/")
            :to-equal "ali0as"))

  (it "ignores multiple prefixes"
    (expect (cljr--alias-here "'#alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "'#0alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "':alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "#':alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "(#':alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "([#'::alias/")
            :to-equal "alias")))

(describe "cljr--unresolved-alias-ref"
  (it "returns unresolved alias reference"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "ns-name")
              :to-equal "ns-name")))

  (it "returns nil for resolved alias"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo (:require [user.ns-name :as ns-name]))")
      (expect (cljr--unresolved-alias-ref "ns-name")
              :to-be nil)))

  (it "returns nil for js alias in cljs file."
    (cljr--with-clojure-temp-file "foo.cljs"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "js")
              :to-be nil)))

  (it "returns js for js alias in clj file."
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "js")
              :to-equal "js")))

  (it "returns js for js alias in cljc file."
    (cljr--with-clojure-temp-file "foo.cljc"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "js")
              :to-equal "js"))))
