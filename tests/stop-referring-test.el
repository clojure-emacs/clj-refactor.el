;;; stop-referring-test.el --- Specs for cljr-stop-referring -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-stop-referring"
  (it "qualifies the referred vars with the namespace"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [|my.lib :refer [a b]]))

(+ (a 1) (b 2))
(+ (A 1) (B 2))"
              (cljr-stop-referring))
            :to-equal "(ns cljr.core
  (:require [my.lib]))

(+ (my.lib/a 1) (my.lib/b 2))
(+ (A 1) (B 2))"))

  (it "qualifies them with the alias when there's one"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [|my.lib :as lib :refer [a b]]))

(+ (a 1) (b 2))
(+ (A 1) (B 2))"
              (cljr-stop-referring))
            :to-equal "(ns cljr.core
  (:require [my.lib :as lib]))

(+ (lib/a 1) (lib/b 2))
(+ (A 1) (B 2))"))

  (it "copes with a newline after :require"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require
    [|my.lib :as lib :refer [a b]]))

(+ (a 1) (b 2))
(+ (A 1) (B 2))"
              (cljr-stop-referring))
            :to-equal "(ns cljr.core
  (:require
    [my.lib :as lib]))

(+ (lib/a 1) (lib/b 2))
(+ (A 1) (B 2))"))

  (it "copes with newlines around :refer"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [|my.lib :as lib
             :refer
             [a b]]))

(+ (a 1) (b 2))
(+ (A 1) (B 2))"
              (cljr-stop-referring))
            :to-equal "(ns cljr.core
  (:require [my.lib :as lib]))

(+ (lib/a 1) (lib/b 2))
(+ (A 1) (B 2))"))

  (it "keeps an :as that comes after the :refer"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [|my.lib :refer [a b] :as lib]))

(+ (a 1) (b 2))
(+ (A 1) (B 2))"
              (cljr-stop-referring))
            :to-equal "(ns cljr.core
  (:require [my.lib :as lib]))

(+ (lib/a 1) (lib/b 2))
(+ (A 1) (B 2))"))

  (it "leaves string literals alone"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [|my.lib :refer [a foo]]))

(def x \"Hello foo\")

(+ (a 1) (foo 2))"
              (cljr-stop-referring))
            :to-equal "(ns cljr.core
  (:require [my.lib]))

(def x \"Hello foo\")

(+ (my.lib/a 1) (my.lib/foo 2))"))

  (it "only touches the libspec point is in"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [other.lib :refer [c]]
            [|my.lib :as lib :refer [a b]]))

(a (b (c 1)))
(abc b-side)"
              (cljr-stop-referring))
            :to-equal "(ns cljr.core
  (:require [other.lib :refer [c]]
            [my.lib :as lib]))

(lib/a (lib/b (c 1)))
(abc b-side)"))

  (it "leaves point where it was"
    (cljr-test-with-buffer "(ns cljr.core
  (:require [my|.lib :refer [a b]]))

(a (b 1))"
      (cljr-stop-referring)
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core
  (:require [my|.lib]))

(my.lib/a (my.lib/b 1))")))

  (it "is bound to sr under the prefix"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [|my.lib :refer [a]]))

(a 1)"
              (cljr-test-keys "C-! sr"))
            :to-equal "(ns cljr.core
  (:require [my.lib]))

(my.lib/a 1)"))

  (it "wants point in a libspec of the :require form"
    (cljr-test-with-buffer "(ns cljr.core
  (:require [my.lib :refer [a]]))

(a |1)"
      (expect (cljr-stop-referring) :to-throw 'user-error))
    (cljr-test-with-buffer "(ns cljr.core
  (:import [|java.util Date]))"
      (expect (cljr-stop-referring) :to-throw 'user-error)))

  (it "fails when the libspec refers nothing"
    (cljr-test-with-buffer "(ns cljr.core
  (:require [|my.lib :as lib]))

(lib/a 1)"
      (expect (cljr-stop-referring) :to-throw 'user-error)
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core
  (:require [|my.lib :as lib]))

(lib/a 1)"))))

;;; stop-referring-test.el ends here
