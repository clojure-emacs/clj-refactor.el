;;; extract-def-test.el --- Specs for cljr-extract-def -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-extract-def"
  (it "defines the form at point after the ns form"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn add-five [a]
  (+ a |5))"
              (cljr-test-call #'cljr-extract-def "five"))
            :to-equal "(ns cljr.core)

(def five 5)

(defn add-five [a]
  (+ a five))"))

  (it "leaves point after the name that replaced the form"
    (expect (cljr-test-with-buffer "(ns cljr.core)

(defn add-five [a]
  (+ a |5))"
              (cljr-test-call #'cljr-extract-def "five")
              (cljr-test-buffer-string :with-point))
            :to-equal "(ns cljr.core)

(def five 5)

(defn add-five [a]
  (+ a five|))"))

  (it "extracts a compound form"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn area [r]
  (* r r |(/ 22 7)))"
              (cljr-test-call #'cljr-extract-def "pi"))
            :to-equal "(ns cljr.core)

(def pi (/ 22 7))

(defn area [r]
  (* r r pi))"))

  (it "offers to replace the other occurrences of the form"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn multiply-by-five [a]
  (* a |5))

(defn add-five [a]
  (+ a 5))"
              (cljr-test-keys "C-! ed" "five" "RET" "y"))
            :to-equal "(ns cljr.core)

(def five 5)

(defn multiply-by-five [a]
  (* a five))

(defn add-five [a]
  (+ a five))"))

  (it "leaves alone the occurrences the user turns down"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn multiply-by-five [a]
  (* a |5))

(defn add-five [a]
  (+ a 5))"
              (cljr-test-keys "C-! ed" "five" "RET" "n"))
            :to-equal "(ns cljr.core)

(def five 5)

(defn multiply-by-five [a]
  (* a five))

(defn add-five [a]
  (+ a 5))"))

  (it "appends to the group of defs that follows the ns form"
    (expect (cljr-test-refactor "(ns cljr.core)

(def one 1)
(def two 2)

(def three 3)

(defn add-five [a]
  (+ a |5))"
              (cljr-test-call #'cljr-extract-def "five"))
            :to-equal "(ns cljr.core)

(def one 1)
(def two 2)

(def three 3)
(def five 5)

(defn add-five [a]
  (+ a five))")))

;;; extract-def-test.el ends here
