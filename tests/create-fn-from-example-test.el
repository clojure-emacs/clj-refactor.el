;;; create-fn-from-example-test.el --- Specs for cljr-create-fn-from-example -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-create-fn (content &optional with-point)
  "Create a function from the example at the `|' in CONTENT.
Return the text of the buffer, with point marked if WITH-POINT."
  (cljr-test-with-buffer content
    (cljr-create-fn-from-example)
    (cljr-test-buffer-string with-point)))

(describe "cljr-create-fn-from-example"
  (it "names the parameters after the arguments"
    (expect (cljr-test-create-fn "(defn some-fn []
  (foo |bar baz))")
            :to-equal "(defn- foo [bar baz]
  )

(defn some-fn []
  (foo bar baz))"))

  (it "copes with a call that spans several lines"
    (expect (cljr-test-create-fn "(defn some-fn []
  (foo
    |bar
    baz))")
            :to-equal "(defn- foo [bar baz]
  )

(defn some-fn []
  (foo
    bar
    baz))"))

  (it "numbers the parameters it can't name"
    (expect (cljr-test-create-fn "(defn some-fn []
  (foo
    \"some string\"
    bar
    {:keyword :value}
    (clojure.string/trim (:a some-map))
    |baz))")
            :to-equal "(defn- foo [arg0 bar arg2 arg3 baz]
  )

(defn some-fn []
  (foo
    \"some string\"
    bar
    {:keyword :value}
    (clojure.string/trim (:a some-map))
    baz))"))

  (it "creates a function without parameters for a call without arguments"
    (expect (cljr-test-create-fn "(defn some-fn []
  (foo|))")
            :to-equal "(defn- foo []
  )

(defn some-fn []
  (foo))"))

  (it "numbers the parameters that stand for literals"
    (expect (cljr-test-create-fn "(defn some-fn []
  (foo| 1 bar :key #{1 2}))")
            :to-equal "(defn- foo [arg0 bar arg2 arg3]
  )

(defn some-fn []
  (foo 1 bar :key #{1 2}))"))

  (it "goes up to the enclosing call when the one at point is a keyword lookup"
    (expect (cljr-test-create-fn "(foo (:bar |baz))")
            :to-equal "(defn- foo [bar]
  )

(foo (:bar baz))"))

  (it "uses the name of the call when point is on something that can't name a function"
    (expect (cljr-test-create-fn "(map :key| items)")
            :to-equal "(defn- map [arg0 items]
  )

(map :key items)"))

  (it "leaves point at the first parameter"
    (expect (cljr-test-create-fn "(defn some-fn []
  (foo |bar baz))"
                                 :with-point)
            :to-equal "(defn- foo [|bar baz]
  )

(defn some-fn []
  (foo bar baz))"))

  (it "leaves point in the body when there are no parameters"
    (expect (cljr-test-create-fn "(foo|)" :with-point)
            :to-equal "(defn- foo []
  |)

(foo)"))

  (it "lets the parameters and the body be filled in one after the other"
    (expect (cljr-test-refactor "(defn some-fn []
  (foo| 1 2))"
              (cljr-test-keys "C-! fe")
              (cljr-test-type "one")
              (cljr-test-keys "<tab>")
              (cljr-test-type "two")
              (cljr-test-keys "<tab>")
              (cljr-test-type "body"))
            :to-equal "(defn- foo [one two]
  body)

(defn some-fn []
  (foo 1 2))"))

  (describe "in a threading macro"
    (it "takes the threaded value first with ->"
      (expect (cljr-test-create-fn "(-> game (reveal|-tile index))")
              :to-equal "(defn- reveal-tile [game index]
  )

(-> game (reveal-tile index))"))

    (it "takes the threaded value last with ->>"
      (expect (cljr-test-create-fn "(->> game (reveal|-tile index))")
              :to-equal "(defn- reveal-tile [index game]
  )

(->> game (reveal-tile index))"))

    (it "takes the threaded value when the function has no parens"
      (expect (cljr-test-create-fn "(-> game do|-stuff)")
              :to-equal "(defn- do-stuff [game]
  )

(-> game do-stuff)")))

  (describe "for a function passed to a core function"
    (it "takes the value under the key with update"
      (expect (cljr-test-create-fn "(update foo :bar do|-stuff)")
              :to-equal "(defn- do-stuff [bar]
  )

(update foo :bar do-stuff)"))

    (it "takes the value under the last key with update-in"
      (expect (cljr-test-create-fn "(update-in foo [:bar :baz] do|-stuff)")
              :to-equal "(defn- do-stuff [baz]
  )

(update-in foo [:bar :baz] do-stuff)"))

    (it "takes the value under the last key with a threaded update-in"
      (expect (cljr-test-create-fn "(-> foo (update-in [:bar :baz] do|-stuff))")
              :to-equal "(defn- do-stuff [baz]
  )

(-> foo (update-in [:bar :baz] do-stuff))"))

    (it "takes one item of each collection with map"
      (expect (cljr-test-create-fn "(map do|-stuff items (:boxes bar))")
              :to-equal "(defn- do-stuff [item box]
  )

(map do-stuff items (:boxes bar))"))

    (it "takes an index and one item of each collection with keep-indexed"
      (expect (cljr-test-create-fn "(keep-indexed do|-stuff foos (:bars baz))")
              :to-equal "(defn- do-stuff [index foo bar]
  )

(keep-indexed do-stuff foos (:bars baz))"))

    (it "takes one item as the keyfn of sort-by"
      (expect (cljr-test-create-fn "(sort-by my|-keyfn comp items)")
              :to-equal "(defn- my-keyfn [item]
  )

(sort-by my-keyfn comp items)"))

    (it "takes two keys as the comparator of sort-by"
      (expect (cljr-test-create-fn "(sort-by :name my|-comp items)")
              :to-equal "(defn- my-comp [name-a name-b]
  )

(sort-by :name my-comp items)"))

    (it "takes two values as the comparator of sort"
      (expect (cljr-test-create-fn "(sort my|-comp [1 2 3])")
              :to-equal "(defn- my-comp [a b]
  )

(sort my-comp [1 2 3])"))

    (it "takes an accumulator and an item with reduce"
      (expect (cljr-test-create-fn "(reduce my|-fold items)")
              :to-equal "(defn- my-fold [acc item]
  )

(reduce my-fold items)"))

    (it "names the accumulator after the initial value of reduce"
      (expect (cljr-test-create-fn "(reduce my|-fold game items)")
              :to-equal "(defn- my-fold [game item]
  )

(reduce my-fold game items)"))

    (it "calls the accumulator acc when the initial value of reduce is a literal"
      (expect (cljr-test-create-fn "(reduce my|-fold {} items)")
              :to-equal "(defn- my-fold [acc item]
  )

(reduce my-fold {} items)"))

    (it "takes nothing with repeatedly"
      (expect (cljr-test-create-fn "(repeatedly 5 make|-thing)")
              :to-equal "(defn- make-thing []
  )

(repeatedly 5 make-thing)")))

  (describe "guessing a parameter name"
    (it "uses the keyword of a lookup"
      (expect (cljr-test-create-fn "(do|-stuff (:foo x) (:bar x))")
              :to-equal "(defn- do-stuff [foo bar]
  )

(do-stuff (:foo x) (:bar x))"))

    (it "uses the last keyword of a threaded lookup"
      (expect (cljr-test-create-fn "(do|-stuff (-> x :foo) (->> x :bar :baz))")
              :to-equal "(defn- do-stuff [foo baz]
  )

(do-stuff (-> x :foo) (->> x :bar :baz))"))

    (it "uses the last key of a get-in"
      (expect (cljr-test-create-fn "(do|-stuff (get-in x [:foo :bar]))")
              :to-equal "(defn- do-stuff [bar]
  )

(do-stuff (get-in x [:foo :bar]))"))

    (it "uses the key of a get, if it's a keyword"
      (expect (cljr-test-create-fn "(do|-stuff (get x :foo) (get x (:bar y)))")
              :to-equal "(defn- do-stuff [foo arg1]
  )

(do-stuff (get x :foo) (get x (:bar y)))"))

    (it "uses what a create- function creates"
      (expect (cljr-test-create-fn "(do|-stuff (create-thing))")
              :to-equal "(defn- do-stuff [thing]
  )

(do-stuff (create-thing))"))

    (it "uses the class of a constructor call"
      (expect (cljr-test-create-fn "(do|-stuff (FileDescriptor. path))")
              :to-equal "(defn- do-stuff [file-descriptor]
  )

(do-stuff (FileDescriptor. path))"))

    (it "drops the package of a constructor call"
      (expect (cljr-test-create-fn "(do|-stuff (java.io.FileDescriptor. path))")
              :to-equal "(defn- do-stuff [file-descriptor]
  )

(do-stuff (java.io.FileDescriptor. path))"))

    (it "uses the property of a Java getter"
      (expect (cljr-test-create-fn "(do|-stuff (.getName f))")
              :to-equal "(defn- do-stuff [name]
  )

(do-stuff (.getName f))"))

    (it "sees through assoc, update and dissoc"
      (expect (cljr-test-create-fn "(do|-stuff (-> game (assoc :foo true) (update :bar inc) (dissoc :baz)))")
              :to-equal "(defn- do-stuff [game]
  )

(do-stuff (-> game (assoc :foo true) (update :bar inc) (dissoc :baz)))"))

    (it "sees through assoc-in and update-in"
      (expect (cljr-test-create-fn "(do|-stuff (-> game (assoc-in [:foo] true) (update-in [:bar] inc)))")
              :to-equal "(defn- do-stuff [game]
  )

(do-stuff (-> game (assoc-in [:foo] true) (update-in [:bar] inc)))"))

    (it "uses the singular for something picked out of a collection"
      (expect (cljr-test-create-fn "(do|-stuff (first things) (second items) (last foos))")
              :to-equal "(defn- do-stuff [thing item foo]
  )

(do-stuff (first things) (second items) (last foos))"))

    (it "uses the plural for something repeated"
      (expect (cljr-test-create-fn "(do|-stuff (repeat 100 (:item stuff)))")
              :to-equal "(defn- do-stuff [items]
  )

(do-stuff (repeat 100 (:item stuff)))"))

    (it "gives up on a form that spans several lines"
      (expect (cljr-test-create-fn "(do|-stuff (first (keep-indexed (fn [index tile]
                      (when (and (= face (:face tile))
                                 (not (:revealed? tile)))
                        index))
                    (:tiles game))))")
              :to-equal "(defn- do-stuff [arg0]
  )

(do-stuff (first (keep-indexed (fn [index tile]
                      (when (and (= face (:face tile))
                                 (not (:revealed? tile)))
                        index))
                    (:tiles game))))")))

  (describe "for a function of another namespace"
    (it "adds a public function at the end of that namespace's file"
      (cljr-test-with-project
          (:name "cljr"
           :files '(("src/cljr/util.clj" . "(ns cljr.util)\n")))
        (spy-on 'cljr--ns-path
                :and-return-value (cljr-test-file "src/cljr/util.clj"))
        (cljr-test-visit "src/cljr/core.clj" "(ns core
  (:require [refactor-nrepl.util :as util]))

(defn some-fn []
  (util/|foo bar baz))")
        (cljr-create-fn-from-example)
        (expect 'cljr--ns-path :to-have-been-called-with "refactor-nrepl.util")
        (expect (cljr-test-file-string "src/cljr/util.clj")
                :to-equal "(ns cljr.util)

(defn foo [bar baz]
  )")
        (expect (cljr-test-file-string "src/cljr/core.clj")
                :to-equal "(ns core
  (:require [refactor-nrepl.util :as util]))

(defn some-fn []
  (util/foo bar baz))")))))

;;; create-fn-from-example-test.el ends here
