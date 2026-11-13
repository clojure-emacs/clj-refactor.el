;;; add-to-ns-test.el --- Specs for adding libspecs to the ns form -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defconst cljr-test-cljc-ns "(ns cljr.core
  #?@(:clj
      [(:require
        [clj-time.core :refer :all]
        [clojure.string :as s])]
      :cljs
      [(:require
        [cljs.string :refer :all])]))"
  "A ns form with a branch for each of Clojure and ClojureScript.")

(describe "cljr-add-require-to-ns"
  (it "adds a require, with the alias typed into the snippet"
    (expect (cljr-test-refactor "|(ns cljr.core)\n"
              (cljr-test-keys "C-! ar" "TAB" "TAB")
              (cljr-test-type "clojure.string")
              (cljr-test-keys "TAB")
              (cljr-test-type "s")
              (cljr-test-keys "TAB"))
            :to-equal "(ns cljr.core
  (:require [clojure.string :as s]))\n"))

  (it "suggests the last part of the namespace as the alias"
    (expect (cljr-test-refactor "|(ns cljr.core)\n"
              (cljr-test-keys "C-! ar" "TAB" "TAB")
              (cljr-test-type "clojure.string")
              (cljr-test-keys "TAB" "TAB"))
            :to-equal "(ns cljr.core
  (:require [clojure.string :as string]))\n"))

  (it "takes a whole libspec typed over the snippet, and sorts the ns form"
    (expect (cljr-test-refactor "|(ns cljr.core
  (:require [clojure.string :as s]))\n"
              (cljr-test-keys "C-! ar")
              (cljr-test-type "[clj-time.core :refer :all]")
              (cljr-test-keys "TAB"))
            :to-equal "(ns cljr.core
  (:require [clj-time.core :refer :all]
            [clojure.string :as s]))\n"))

  (it "leaves the libspecs in the order they were added when not sorting"
    (let ((cljr-auto-sort-ns nil))
      (expect (cljr-test-refactor "|(ns cljr.core
  (:require [clojure.string :as s]))\n"
                (cljr-test-keys "C-! ar")
                (cljr-test-type "[clj-time.core :refer :all]")
                (cljr-test-keys "TAB"))
              :to-equal "(ns cljr.core
  (:require [clojure.string :as s]
            [clj-time.core :refer :all]))\n")))

  (it "goes back to where point was once the snippet is done"
    (cljr-test-with-buffer "(ns cljr.core)\n\n(s/join| xs)"
      (cljr-test-keys "C-! ar" "TAB" "TAB")
      (cljr-test-type "clojure.string")
      (cljr-test-keys "TAB")
      (cljr-test-type "s")
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core
  (:require [clojure.string :as s|]))

(s/join xs)")
      (cljr-test-keys "TAB")
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core
  (:require [clojure.string :as s]))

(s/join| xs)")))

  (it "adds to the cljs branch with a prefix argument"
    (expect (cljr-test-refactor (concat "|" cljr-test-cljc-ns)
              (cljr-test-keys "C-u C-! ar" "TAB" "TAB")
              (cljr-test-type "cljs-time.core")
              (cljr-test-keys "TAB")
              (cljr-test-type "t"))
            :to-equal "(ns cljr.core
  #?@(:clj
      [(:require
        [clj-time.core :refer :all]
        [clojure.string :as s])]
      :cljs
      [(:require
        [cljs.string :refer :all]
        [cljs-time.core :as t])]))"))

  (it "fails when the buffer has no ns form"
    (cljr-test-with-buffer "|(defn f [])"
      (expect (cljr-add-require-to-ns nil) :to-throw 'error)
      (expect (cljr-test-buffer-string) :to-equal "(defn f [])")))

  (it "fails when asked for a cljs branch the ns form doesn't have"
    (cljr-test-with-buffer "|(ns cljr.core)\n"
      (expect (cljr-add-require-to-ns t) :to-throw 'error)
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core)\n"))))

(describe "cljr-add-use-to-ns"
  (it "adds a require that refers everything"
    (expect (cljr-test-refactor "|(ns cljr.core)\n"
              (cljr-test-keys "C-! au")
              (cljr-test-type "clj-time.core")
              (cljr-test-keys "TAB")
              (cljr-test-type ":all"))
            :to-equal "(ns cljr.core
  (:require [clj-time.core :refer :all]))\n"))

  (it "adds a require that refers the names typed into the vector"
    (cljr-test-with-buffer "|(ns cljr.core
  (:require [clj-time.core :refer :all]))\n"
      (cljr-test-keys "C-! au")
      (cljr-test-type "clojure.string")
      (cljr-test-keys "TAB" "TAB")
      (cljr-test-type "join")
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core
  (:require [clj-time.core :refer :all]
            [clojure.string :refer [join|]]))\n")))

  (it "adds to the cljs branch with a prefix argument"
    (expect (cljr-test-refactor (concat "|" cljr-test-cljc-ns)
              (cljr-test-keys "C-u C-! au")
              (cljr-test-type "cljs-time.core")
              (cljr-test-keys "TAB")
              (cljr-test-type ":all"))
            :to-equal "(ns cljr.core
  #?@(:clj
      [(:require
        [clj-time.core :refer :all]
        [clojure.string :as s])]
      :cljs
      [(:require
        [cljs.string :refer :all]
        [cljs-time.core :refer :all])]))")))

(describe "cljr-add-import-to-ns"
  (it "adds an :import form for the first class"
    (expect (cljr-test-refactor "|(ns cljr.core)\n"
              (cljr-test-keys "C-! ai")
              (cljr-test-type "java.io.File")
              (cljr-test-keys "TAB"))
            :to-equal "(ns cljr.core
  (:import java.io.File))\n"))

  (it "adds to the :import form that's there"
    (cljr-test-with-buffer "|(ns cljr.core
  (:import java.io.File))\n"
      (cljr-test-keys "C-! ai")
      (cljr-test-type "[java.util Date]")
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core
  (:import java.io.File
           [java.util Date]|))\n")))

  (it "adds an :import form to the cljs branch with a prefix argument"
    (expect (cljr-test-refactor "|(ns cljr.core
  #?@(:clj
      [(:require
        [clj-time.core :refer :all]
        [clojure.string :as s])
       (:import java.util.Date)]
      :cljs
      [(:require
        [cljs.string :refer :all])]))"
              (cljr-test-keys "C-u C-! ai")
              (cljr-test-type "goog.string")
              (cljr-test-keys "TAB"))
            :to-equal "(ns cljr.core
  #?@(:clj
      [(:require
        [clj-time.core :refer :all]
        [clojure.string :as s])
       (:import java.util.Date)]
      :cljs
      [(:require
        [cljs.string :refer :all])
       (:import goog.string)]))")))

(describe "cljr-require-macro"
  (it "adds a :require-macros form"
    (expect (cljr-test-refactor "|(ns cljr.core)\n"
              (cljr-test-keys "C-! rm" "TAB" "TAB")
              (cljr-test-type "clojure.test")
              (cljr-test-keys "TAB")
              (cljr-test-type "t")
              (cljr-test-keys "TAB"))
            :to-equal "(ns cljr.core
  (:require-macros [clojure.test :as t]))\n"))

  (it "doesn't take the :require form for the :require-macros one"
    (expect (cljr-test-refactor "|(ns cljr.core
  (:require [clojure.string :as s]))\n"
              (cljr-test-keys "C-! rm" "TAB" "TAB")
              (cljr-test-type "clojure.test")
              (cljr-test-keys "TAB")
              (cljr-test-type "t")
              (cljr-test-keys "TAB"))
            :to-equal "(ns cljr.core
  (:require [clojure.string :as s])
  (:require-macros [clojure.test :as t]))\n")))

;;; add-to-ns-test.el ends here
