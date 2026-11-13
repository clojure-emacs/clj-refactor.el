;;; move-form-test.el --- Specs for cljr-move-form -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defmacro cljr-test-with-move-project (&rest body)
  "Evaluate BODY in a project that has a place to move forms to."
  (declare (indent 0) (debug t))
  `(cljr-test-with-project
       (:name "cljr"
        :files '(("src/cljr/dest.clj" . "(ns cljr.dest)

(defn frobinator [a b]
  (+ a b))")
                 ("src/cljr/target.clj" . "(ns cljr.target)

(defn really-do-it [x] (println x))")))
     ,@body))

(describe "cljr-move-form"
  (before-each
    ;; Cleaning the ns form is the middleware's job.
    (spy-on 'cljr--clean-or-sort-ns))

  (it "moves the top-level form around point and refers to it from where it was"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src)

(defn| add [a b]
  (+ a b))

(add 1 2)")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src
  (:require [cljr.dest :refer [add]]))

(add 1 2)")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest)

(defn frobinator [a b]
  (+ a b))

(defn add [a b]
  (+ a b))
")))

  (it "takes the requires of the ns along"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src
  (:require [clojure.string :as str]))

(defn| foo [y x]
  (str/join x y))

(foo 1 2)")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src
  (:require [clojure.string :as str]
            [cljr.dest :refer [foo]]))

(foo 1 2)")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest
  (:require [clojure.string :as str]))

(defn frobinator [a b]
  (+ a b))

(defn foo [y x]
  (str/join x y))
")))

  (it "moves all the forms in the region"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src)

(defn add [a b]
  (+ a b))

(defn sub [a b]
  (- a b))

(defn mult [a b]
  (* a b))

(defn div [a b]
  (/ a b))

(div (add 1 (sub 1 2)) (mult 1 3))")
      (cljr-test-select "(defn add" "(defn div")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src
  (:require [cljr.dest :refer [add sub mult]]))

(defn div [a b]
  (/ a b))

(div (add 1 (sub 1 2)) (mult 1 3))")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest)

(defn frobinator [a b]
  (+ a b))

(defn add [a b]
  (+ a b))

(defn sub [a b]
  (- a b))

(defn mult [a b]
  (* a b))
")))

  (it "adds the require after the ones that refer to other namespaces"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src
  (:require [clojure.string :refer [join split]]))

(defn| add [a b]
  (+ a b))

(add (this 1) (that 2))")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src
  (:require [clojure.string :refer [join split]]
            [cljr.dest :refer [add]]))

(add (this 1) (that 2))")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest
  (:require [clojure.string :refer [join split]]))

(defn frobinator [a b]
  (+ a b))

(defn add [a b]
  (+ a b))
")))

  (it "adds to what's referred from the target already, leaving private functions out"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns ^{:meta \"...\"}
  cljr.foo
  \"doc...\"
  (:use [cljr.core]
        [cljr.page]
        [cljr.element]
        [cljr.form]
        [some.lib ns1 ns2 ns3])
  (:require [cljr.foobar :as foo]
            [clojure.string :refer [join split]]
            [cljr.dest :refer [this that]])
  (:refer-clojure :exclude [this that])
  (:import [java.util.Date]))

(defn select
  \"Returns a set of the elements for which pred is true\"
  {:added \"1.0\"}
  [pred xset]
    (reduce (fn [s k] (if (pred k) s (disj s k)))
            xset xset))

(defn- add
  \"docstring\"
  {:map \"with\" :random \"facts\"}
  [a b]
  (+ a b))

(defroute create \"/*/create\")
(defroute update \"/*/update/:id\")

(defn find-doc
  \"Prints documentation for any var whose documentation or name
 contains a match for re-string-or-pattern\"
  {:added \"1.0\"}
  [re-string-or-pattern]
    (let [re  (re-pattern re-string-or-pattern)]
      (doseq [ns (all-ns)
              v (sort-by (comp :name meta) (vals (ns-interns ns)))
              :when (and (:doc (meta v))
                         (or (re-find (re-matcher re (:doc (meta v))))
                             (re-find (re-matcher re (str (:name (meta v)))))))]
               (print-doc v))))

(defn rename
  \"Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap\"
  {:added \"1.0\"}
  [xrel kmap]
    (set (map #(rename-keys kmap) xrel)))

(select (find-doc (foo/those)))")
      (cljr-test-select "(defn select" "(defn rename")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns ^{:meta \"...\"}
  cljr.foo
  \"doc...\"
  (:use [cljr.core]
        [cljr.page]
        [cljr.element]
        [cljr.form]
        [some.lib ns1 ns2 ns3])
  (:require [cljr.foobar :as foo]
            [clojure.string :refer [join split]]
            [cljr.dest :refer [this that select find-doc]])
  (:refer-clojure :exclude [this that])
  (:import [java.util.Date]))

(defn rename
  \"Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap\"
  {:added \"1.0\"}
  [xrel kmap]
    (set (map #(rename-keys kmap) xrel)))

(select (find-doc (foo/those)))")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest
  (:require [cljr.foobar :as foo]
            [clojure.string :refer [join split]]))

(defn frobinator [a b]
  (+ a b))

(defn select
  \"Returns a set of the elements for which pred is true\"
  {:added \"1.0\"}
  [pred xset]
    (reduce (fn [s k] (if (pred k) s (disj s k)))
            xset xset))

(defn- add
  \"docstring\"
  {:map \"with\" :random \"facts\"}
  [a b]
  (+ a b))

(defroute create \"/*/create\")
(defroute update \"/*/update/:id\")

(defn find-doc
  \"Prints documentation for any var whose documentation or name
 contains a match for re-string-or-pattern\"
  {:added \"1.0\"}
  [re-string-or-pattern]
    (let [re  (re-pattern re-string-or-pattern)]
      (doseq [ns (all-ns)
              v (sort-by (comp :name meta) (vals (ns-interns ns)))
              :when (and (:doc (meta v))
                         (or (re-find (re-matcher re (:doc (meta v))))
                             (re-find (re-matcher re (str (:name (meta v)))))))]
               (print-doc v))))
")))

  (it "saves the target file and leaves the user in the file the form came from"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src)

(defn| add [a b]
  (+ a b))

(add 1 2)")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (buffer-file-name) :to-equal (cljr-test-file "src/cljr/src.clj"))
      (expect (buffer-modified-p
               (find-buffer-visiting (cljr-test-file "src/cljr/dest.clj")))
              :to-be nil)))

  (it "doesn't make the target require itself, and drops its alias from the moved code"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src
  (:require [cljr.target :as t]
            [clojure.set :as st]
            [clojure.string :as str]))

(defn| doit [x]
  (str/join (st/union '(\"a\" \"b\")))
  (t/really-do-it x))

(doit 1)")
      (cljr-test-call #'cljr-move-form "target.clj")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src
  (:require [cljr.target :as t :refer [doit]]
            [clojure.set :as st]
            [clojure.string :as str]))

(doit 1)")
      (expect (cljr-test-file-string "src/cljr/target.clj")
              :to-equal "(ns cljr.target
  (:require [clojure.set :as st]
            [clojure.string :as str]))

(defn really-do-it [x] (println x))

(defn doit [x]
  (str/join (st/union '(\"a\" \"b\")))
  (really-do-it x))
")))

  (it "requires nothing in the target when the target was all the form needed"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src
  (:require [cljr.target :as t]))

(defn| doit [x]
  (t/really-do-it x))

(doit 1)")
      (cljr-test-call #'cljr-move-form "target.clj")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src
  (:require [cljr.target :as t :refer [doit]]))

(doit 1)")
      (expect (cljr-test-file-string "src/cljr/target.clj")
              :to-equal "(ns cljr.target)

(defn really-do-it [x] (println x))

(defn doit [x]
  (really-do-it x))
")))

  (it "moves the last form of a file"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src)

(defn| add [a b]
  (+ a b))
")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (cljr-test-buffer-string t)
              :to-equal "(ns cljr.src
  (:require [cljr.dest :refer [add]]))
|")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest)

(defn frobinator [a b]
  (+ a b))

(defn add [a b]
  (+ a b))
")))

  (it "moves the last form of a file that doesn't end in a newline"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src)

(defn| add [a b]
  (+ a b))")
      (cljr-test-call #'cljr-move-form "dest.clj")
      (expect (cljr-test-buffer-string) :not :to-match "defn add")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest)

(defn frobinator [a b]
  (+ a b))

(defn add [a b]
  (+ a b))
")))

  (it "refuses to move a region that cuts a form in two"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src)

(defn add [a b]
  (+ a b))

(add 1 2)")
      (cljr-test-select "(defn add" "(+ a b)")
      (expect (cljr-move-form) :to-throw)
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src)

(defn add [a b]
  (+ a b))

(add 1 2)")
      (expect (cljr-test-file-string "src/cljr/dest.clj")
              :to-equal "(ns cljr.dest)

(defn frobinator [a b]
  (+ a b))")))

  (it "is bound to mf under the prefix"
    (cljr-test-with-move-project
      (cljr-test-visit "src/cljr/src.clj" "(ns cljr.src)

(defn| add [a b]
  (+ a b))

(add 1 2)")
      (cljr-test-keys "C-! mf" "dest.clj RET")
      (expect (cljr-test-buffer-string)
              :to-equal "(ns cljr.src
  (:require [cljr.dest :refer [add]]))

(add 1 2)"))))

;;; move-form-test.el ends here
