;;; extract-constant-test.el --- Specs for cljr-extract-constant -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-extract-constant"
  (it "defines the form at point as a constant after the ns form"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn add-five [a]
  (+ a |5))"
              (cljr-test-call #'cljr-extract-constant "five"))
            :to-equal "(ns cljr.core)

(def ^:const five 5)

(defn add-five [a]
  (+ a five))"))

  (it "offers to replace the other occurrences of the form"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn multiply-by-five [a]
  (* a |5))

(defn add-five [a]
  (+ a 5))"
              (cljr-test-keys "C-! ec" "five" "RET" "y"))
            :to-equal "(ns cljr.core)

(def ^:const five 5)

(defn multiply-by-five [a]
  (* a five))

(defn add-five [a]
  (+ a five))"))

  (it "offers to replace the occurrences before the one at point too"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn multiply-by-five [a]
  (* a 5))

(defn add-five [a]
  (+ a |5))"
              (cljr-test-keys "C-! ec" "five" "RET" "y"))
            :to-equal "(ns cljr.core)

(def ^:const five 5)

(defn multiply-by-five [a]
  (* a five))

(defn add-five [a]
  (+ a five))"))

  (it "appends to the group of defs that follows the ns form"
    (expect (cljr-test-refactor "(ns cljr.core)

(def one 1)

(defn add-five [a]
  (+ a |5))"
              (cljr-test-call #'cljr-extract-constant "five"))
            :to-equal "(ns cljr.core)

(def one 1)
(def ^:const five 5)

(defn add-five [a]
  (+ a five))")))

;;; extract-constant-test.el ends here
