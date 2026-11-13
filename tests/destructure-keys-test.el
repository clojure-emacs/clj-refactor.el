;;; destructure-keys-test.el --- Specs for cljr-destructure-keys -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-destructure-keys"
  (it "destructures a parameter that's only used for its keys"
    (expect (cljr-test-refactor "(defn f [|o]
  (+ (:a1 o) (:a2 o)))"
              (cljr-destructure-keys))
            :to-equal "(defn f [{:keys [a1 a2]}]
  (+ a1 a2))"))

  (it "keeps the map around with :as when it's used for more than its keys"
    (expect (cljr-test-refactor "(defn f [|o]
  (+ (:a1 o) (some-fn o)))"
              (cljr-destructure-keys))
            :to-equal "(defn f [{:keys [a1] :as o}]
  (+ a1 (some-fn o)))"))

  (it "destructures a let binding, in the bindings that follow it and in the body"
    (expect (cljr-test-refactor "(let [|a (get-obj)
      b (:p1 a)
      c (:p2 a)]
  (+ b c (:p3 a)))"
              (cljr-destructure-keys))
            :to-equal "(let [{:keys [p1 p2 p3]} (get-obj)
      b p1
      c p2]
  (+ b c p3))"))

  (it "lists a key that's used several times once"
    (expect (cljr-test-refactor "(defn f [|o]
  (+ (:a1 o) (:a1 o)))"
              (cljr-destructure-keys))
            :to-equal "(defn f [{:keys [a1]}]
  (+ a1 a1))"))

  (it "works with point inside or at the end of the symbol"
    (expect (cljr-test-refactor "(defn f [ob|j]
  (+ (:a1 obj) (:a2 obj)))"
              (cljr-destructure-keys))
            :to-equal "(defn f [{:keys [a1 a2]}]
  (+ a1 a2))")
    (expect (cljr-test-refactor "(defn f [obj|]
  (+ (:a1 obj) (:a2 obj)))"
              (cljr-destructure-keys))
            :to-equal "(defn f [{:keys [a1 a2]}]
  (+ a1 a2))"))

  (it "leaves the other parameters and their keys alone"
    (expect (cljr-test-refactor "(defn f [|o p]
  (+ (:a1 o) (:a2 p)))"
              (cljr-destructure-keys))
            :to-equal "(defn f [{:keys [a1]} p]
  (+ a1 (:a2 p)))"))

  (it "leaves point after the destructuring form"
    (expect (cljr-test-with-buffer "(defn f [|o]
  (+ (:a1 o) (:a2 o)))"
              (cljr-destructure-keys)
              (cljr-test-buffer-string t))
            :to-equal "(defn f [{:keys [a1 a2]}|]
  (+ a1 a2))"))

  (it "refuses to work on a symbol that isn't in a binding vector"
    (cljr-test-with-buffer "(defn f [o]
  (+ (:a1 |o) (:a2 o)))"
      (expect (cljr-destructure-keys) :to-throw 'user-error)
      (expect (cljr-test-buffer-string t)
              :to-equal "(defn f [o]
  (+ (:a1 |o) (:a2 o)))")))

  (it "is bound to dk under the prefix"
    (expect (cljr-test-refactor "(defn f [|o]
  (+ (:a1 o) (:a2 o)))"
              (cljr-test-keys "C-! dk"))
            :to-equal "(defn f [{:keys [a1 a2]}]
  (+ a1 a2))")))

;;; destructure-keys-test.el ends here
