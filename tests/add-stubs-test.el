;;; add-stubs-test.el --- Specs for adding stubs of interfaces and protocols -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defconst cljr-test-list-stubs
  "({:parameter-list \"[^int arg]\", :name \"remove\"}
{:parameter-list \"[^int arg0 ^Object arg1]\", :name \"add\"}
{:parameter-list \"[^java.util.function.UnaryOperator arg]\", :name \"replaceAll\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"containsAll\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"removeAll\"}
{:parameter-list \"[]\", :name \"listIterator\"}
{:parameter-list \"[^int arg0 ^int arg1]\", :name \"subList\"}
{:parameter-list \"[]\", :name \"iterator\"}
{:parameter-list \"[^Object arg]\", :name \"lastIndexOf\"}
{:parameter-list \"[^int arg]\", :name \"listIterator\"}
{:parameter-list \"[^int arg0 ^java.util.Collection arg1]\", :name \"addAll\"}
{:parameter-list \"[^Object arg]\", :name \"add\"}
{:parameter-list \"[^int arg]\", :name \"get\"}
{:parameter-list \"[]\", :name \"toArray\"}
{:parameter-list \"[]\", :name \"clear\"}
{:parameter-list \"[^int arg0 ^Object arg1]\", :name \"set\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"retainAll\"}
{:parameter-list \"[]\", :name \"isEmpty\"}
{:parameter-list \"[^java.util.Collection arg]\", :name \"addAll\"}
{:parameter-list \"[]\", :name \"spliterator\"}
{:parameter-list \"[^Object arg]\", :name \"indexOf\"}
{:parameter-list \"[^Object... arg]\", :name \"toArray\"}
{:parameter-list \"[^java.util.Comparator arg]\", :name \"sort\"}
{:parameter-list \"[]\", :name \"size\"}
{:parameter-list \"[^Object arg]\", :name \"equals\"}
{:parameter-list \"[^Object arg]\", :name \"remove\"}
{:parameter-list \"[]\", :name \"hashCode\"}
{:parameter-list \"[^Object arg]\", :name \"contains\"})"
  "What the stubs-for-interface op answers with for java.util.List.")

(describe "cljr-add-stubs"
  (before-each
    (cljr-test-without-middleware))

  (it "adds a stub for each method of the interface at point"
    (cljr-test-middleware-answers cljr-test-list-stubs)
    (expect (cljr-test-refactor "(ns cljr.core)

(defrecord MyRecord []
  java.util.|List)"
              (cljr-add-stubs))
            :to-equal "(ns cljr.core)

(defrecord MyRecord []
  java.util.List
  (remove [^int arg])
  (add [^int arg0 ^Object arg1])
  (replaceAll [^java.util.function.UnaryOperator arg])
  (containsAll [^java.util.Collection arg])
  (removeAll [^java.util.Collection arg])
  (listIterator [])
  (subList [^int arg0 ^int arg1])
  (iterator [])
  (lastIndexOf [^Object arg])
  (listIterator [^int arg])
  (addAll [^int arg0 ^java.util.Collection arg1])
  (add [^Object arg])
  (get [^int arg])
  (toArray [])
  (clear [])
  (set [^int arg0 ^Object arg1])
  (retainAll [^java.util.Collection arg])
  (isEmpty [])
  (addAll [^java.util.Collection arg])
  (spliterator [])
  (indexOf [^Object arg])
  (toArray [^Object... arg])
  (sort [^java.util.Comparator arg])
  (size [])
  (equals [^Object arg])
  (remove [^Object arg])
  (hashCode [])
  (contains [^Object arg]))")
    (expect (cljr-test-middleware-request "interface") :to-equal "java.util.List"))

  (it "looks up a protocol through the alias of its namespace"
    (cljr-test-middleware-answers "({:parameter-list \"[this]\", :name \"typename\"})")
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [clojure.repl :as repl]
            [clojure.reflect :as r]))

(defrecord MyRecord []
  r/|TypeReference)"
              (cljr-add-stubs))
            :to-equal "(ns cljr.core
  (:require [clojure.repl :as repl]
            [clojure.reflect :as r]))

(defrecord MyRecord []
  r/TypeReference
  (typename [this]))")
    (expect (cljr-test-middleware-request "interface")
            :to-equal "clojure.reflect/TypeReference"))

  (it "looks up an unqualified protocol in the namespace of the buffer"
    (cljr-test-middleware-answers "({:parameter-list \"[this]\", :name \"start\"})")
    (cljr-test-with-buffer "(ns cljr.core)

(deftype MyType []
  |Lifecycle)"
      (cljr-add-stubs))
    (expect (cljr-test-middleware-request "interface") :to-equal "cljr.core/Lifecycle"))

  (it "leaves point where the body of the first stub goes"
    (cljr-test-middleware-answers "({:parameter-list \"[this]\", :name \"start\"}
{:parameter-list \"[this]\", :name \"stop\"})")
    (expect (cljr-test-with-buffer "(ns cljr.core)

(deftype MyType []
  |Lifecycle)"
              (cljr-add-stubs)
              (cljr-test-buffer-string t))
            :to-equal "(ns cljr.core)

(deftype MyType []
  Lifecycle
  (start [this]|)
  (stop [this]))"))

  (it "keeps the stubs of the interfaces that follow where they are"
    (cljr-test-middleware-answers "({:parameter-list \"[this]\", :name \"start\"})")
    (expect (cljr-test-refactor "(ns cljr.core)

(deftype MyType []
  Life|cycle
  Runnable
  (run [this]))"
              (cljr-add-stubs))
            :to-equal "(ns cljr.core)

(deftype MyType []
  Lifecycle
  (start [this])
  Runnable
  (run [this]))"))

  (it "needs an ns form to look the interface up in"
    (cljr-test-middleware-answers "({:parameter-list \"[this]\", :name \"start\"})")
    (cljr-test-with-buffer "(deftype MyType []
  |Lifecycle)"
      (expect (cljr-add-stubs) :to-throw 'error)
      (expect (cljr-test-buffer-string t) :to-equal "(deftype MyType []
  |Lifecycle)"))
    (expect 'cljr--call-middleware-sync :not :to-have-been-called))

  (it "adds nothing when the interface has no methods"
    (cljr-test-middleware-answers "()")
    (expect (cljr-test-with-buffer "(ns cljr.core)

(deftype MyType []
  |java.io.Serializable)"
              (cljr-add-stubs)
              (cljr-test-buffer-string t))
            :to-equal "(ns cljr.core)

(deftype MyType []
  java.io.Serializable|)")))

;;; add-stubs-test.el ends here
