;;; add-declaration-test.el --- Specs for cljr-add-declaration -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-add-declaration"
  (it "declares the current def after the ns form"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn|- ^:meta add [a b]
  (+ a b))"
              (cljr-add-declaration nil))
            :to-equal "(ns cljr.core)

(declare add)

(defn- ^:meta add [a b]
  (+ a b))"))

  (it "adds to the declare that's already there"
    (expect (cljr-test-refactor "(ns cljr.core)

(declare foo)

(defn|- ^{:meta :data} add
  [a b]
  (+ a b))"
              (cljr-add-declaration nil))
            :to-equal "(ns cljr.core)

(declare foo add)

(defn- ^{:meta :data} add
  [a b]
  (+ a b))"))

  (it "declares a def made with a namespaced macro"
    (expect (cljr-test-refactor "(ns cljr.core)

(s/defn| foo
  [a b]
  (+ a b))"
              (cljr-add-declaration nil))
            :to-equal "(ns cljr.core)

(declare foo)

(s/defn foo
  [a b]
  (+ a b))"))

  (it "declares the symbol at point outside a def"
    (expect (cljr-test-refactor "(ns cljr.core)

(foo| :bar)"
              (cljr-add-declaration nil))
            :to-equal "(ns cljr.core)

(declare foo)

(foo :bar)"))

  (it "declares the symbol at point inside a def with a prefix argument"
    (expect (cljr-test-refactor "(ns cljr.core)

(declare foo)

(defn- ^{:meta :data} add
  [a b]
  (bar| a b))"
              (cljr-test-keys "C-u C-! ad"))
            :to-equal "(ns cljr.core)

(declare foo bar)

(defn- ^{:meta :data} add
  [a b]
  (bar a b))"))

  (it "leaves point where it was"
    (expect (cljr-test-with-buffer "(ns cljr.core)

(defn add [a b]
  (+ a| b))"
              (cljr-add-declaration nil)
              (cljr-test-buffer-string :with-point))
            :to-equal "(ns cljr.core)

(declare add)

(defn add [a b]
  (+ a| b))"))

  (it "refuses to declare something twice"
    (cljr-test-with-buffer "(ns cljr.core)

(declare foo add)

(defn add [a b]
  (+ a| b))"
      (expect (cljr-add-declaration nil) :to-throw 'user-error)
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core)

(declare foo add)

(defn add [a b]
  (+ a b))")))

  (it "declares a name that a declared one merely contains"
    (expect (cljr-test-refactor "(ns cljr.core)

(declare valid?*)

(defn valid? [x]
  |x)"
              (cljr-add-declaration nil))
            :to-equal "(ns cljr.core)

(declare valid?* valid?)

(defn valid? [x]
  x)")))

;;; add-declaration-test.el ends here
