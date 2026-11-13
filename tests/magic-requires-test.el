;;; magic-requires-test.el --- Specs for the requires that typing / adds -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-suggest-libspecs (alias-ref _language-context)
  "Return what the middleware would suggest requiring for ALIAS-REF."
  (pcase alias-ref
    ("set" '("[clojure.set :as set]"))
    ("util" '("[refactor-nrepl.util :as util]"
              "[clojure.tools.analyzer.jvm.utils :as util]"))))

(describe "typing / after an alias"
  (before-each
    (cljr-test-without-middleware)
    (spy-on 'cljr--call-middleware-suggest-libspec
            :and-call-fake #'cljr-test-suggest-libspecs))

  (it "requires the namespace the middleware suggests for it"
    (cljr-test-with-buffer "(ns cljr.core)

(set|)"
      (cljr-test-type "/union")
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core
  (:require [clojure.set :as set]))

(set/union|)")
      (expect 'cljr--call-middleware-suggest-libspec
              :to-have-been-called-with "set" '("clj" nil))))

  (it "asks which namespace to require when several are suggested"
    (expect (cljr-test-refactor "(ns cljr.core)

(util|)"
              (cljr-test-type "/[refactor-nrepl.util :as util]\rget-last-sexp"))
            :to-equal "(ns cljr.core
  (:require [refactor-nrepl.util :as util]))

(util/get-last-sexp)"))

  (it "requires the namespace for an auto-resolved keyword"
    (expect (cljr-test-refactor "(ns cljr.core)

(::util|)"
              (cljr-test-type "/[refactor-nrepl.util :as util]\rsome-keyword"))
            :to-equal "(ns cljr.core
  (:require [refactor-nrepl.util :as util]))

(::util/some-keyword)"))

  (it "adds to the :require form that's there"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [clojure.string :as str]))

(set|)"
              (cljr-test-type "/union"))
            :to-equal "(ns cljr.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(set/union)"))

  (it "requires nothing when the alias is taken"
    (expect (cljr-test-refactor "(ns cljr.core
  (:require [refactor-nrepl.util :as util]))

(util|)"
              (cljr-test-type "/get-last-sexp"))
            :to-equal "(ns cljr.core
  (:require [refactor-nrepl.util :as util]))

(util/get-last-sexp)")
    (expect 'cljr--call-middleware-suggest-libspec :not :to-have-been-called))

  (it "requires nothing when the middleware has no suggestion"
    (expect (cljr-test-refactor "(ns cljr.core)

(nope|)"
              (cljr-test-type "/f"))
            :to-equal "(ns cljr.core)

(nope/f)"))

  (it "requires nothing in a buffer that uses in-ns"
    (expect (cljr-test-refactor "(in-ns 'cljr.core)

(set|)"
              (cljr-test-type "/union"))
            :to-equal "(in-ns 'cljr.core)

(set/union)"))

  (it "requires nothing in a buffer without a ns form"
    (expect (cljr-test-refactor "(set|)"
              (cljr-test-type "/union"))
            :to-equal "(set/union)"))

  (it "requires nothing when destructuring a map"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn f [{:keys [set|]}])"
              (cljr-test-type "/union"))
            :to-equal "(ns cljr.core)

(defn f [{:keys [set/union]}])"))

  (it "requires nothing for a keyword that isn't auto-resolved"
    (expect (cljr-test-refactor "(ns cljr.core)

(:set|)"
              (cljr-test-type "/union"))
            :to-equal "(ns cljr.core)

(:set/union)"))

  (it "requires nothing in a string"
    (expect (cljr-test-refactor "(ns cljr.core)

(def path \"set|\")"
              (cljr-test-type "/union"))
            :to-equal "(ns cljr.core)

(def path \"set/union\")"))

  (it "requires nothing in a comment"
    (expect (cljr-test-refactor "(ns cljr.core)

;; set|"
              (cljr-test-type "/union"))
            :to-equal "(ns cljr.core)

;; set/union")))

;;; magic-requires-test.el ends here
