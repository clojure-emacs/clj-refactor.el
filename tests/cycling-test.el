;;; cycling-test.el --- Specs for cycling between forms -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-cycle-thread"
  (it "turns a thread-first into a thread-last and reindents it"
    (expect (cljr-test-refactor "(defn foo [coll]
  (-> coll
      (map| inc)))"
              (cljr-cycle-thread))
            :to-equal "(defn foo [coll]
  (->> coll
       (map inc)))"))

  (it "turns a thread-last into a thread-first and reindents it"
    (expect (cljr-test-refactor "(defn foo [coll]
  (->> coll
       (map| inc)))"
              (cljr-cycle-thread))
            :to-equal "(defn foo [coll]
  (-> coll
      (map inc)))"))

  (it "only touches the innermost of two nested threads"
    (expect (cljr-test-refactor "(defn foo []
  (-> {}
      (->> (assoc| :bar 1))))"
              (cljr-cycle-thread))
            :to-equal "(defn foo []
  (-> {}
      (-> (assoc :bar 1))))"))

  (it "cycles the other threading macros too"
    (expect (cljr-test-refactor "(cond-> m
  flag| (assoc :a 1))"
              (cljr-cycle-thread))
            :to-equal "(cond->> m
  flag (assoc :a 1))")
    (expect (cljr-test-refactor "(some->> m
         |(map inc))"
              (cljr-cycle-thread))
            :to-equal "(some-> m
        (map inc))"))

  (it "leaves point on the form it was on"
    (expect (cljr-test-with-buffer "(-> coll
    (map| inc))"
              (cljr-cycle-thread)
              (cljr-test-buffer-string t))
            :to-equal "(->> coll
     (map| inc))"))

  (it "is bound to ct under the prefix"
    (expect (cljr-test-refactor "(-> coll
    (map| inc))"
              (cljr-test-keys "C-! ct"))
            :to-equal "(->> coll
     (map inc))")))

;;; cycling-test.el ends here
