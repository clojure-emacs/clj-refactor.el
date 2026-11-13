;;; magic-requires-cljc-test.el --- Specs for the requires that typing / adds in cljc files -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-suggest-pprint-libspecs (alias-ref language-context)
  "Return what the middleware would suggest requiring for ALIAS-REF.
It's the Clojure or the ClojureScript pretty printer, or both, going by
LANGUAGE-CONTEXT: the language of the file, and of the reader
conditional branch point is in."
  (when (equal alias-ref "pprint")
    (pcase (or (cadr language-context) (car language-context))
      ("clj" '("[clojure.pprint :as pprint]"))
      ("cljs" '("[cljs.pprint :as pprint]"))
      (_ '("[clojure.pprint :as pprint]"
           "[cljs.pprint :as pprint]")))))

(defmacro cljr-test-with-cljc-file (content &rest body)
  "Evaluate BODY in a buffer visiting a cljc file that holds CONTENT."
  (declare (indent 1) (debug t))
  `(cljr-test-with-project (:name "cljr")
     (cljr-test-visit "src/cljr/core.cljc" ,content)
     ,@body))

(describe "typing / after an alias in a cljc file"
  (before-each
    (cljr-test-without-middleware)
    (spy-on 'cljr--call-middleware-suggest-libspec
            :and-call-fake #'cljr-test-suggest-pprint-libspecs))

  (it "requires the Clojure namespace in a :clj branch"
    (cljr-test-with-cljc-file "(ns cljr.core)

#?(:clj (pprint|))"
      (cljr-test-type "/pprint")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core
  (:require [clojure.pprint :as pprint]))

#?(:clj (pprint/pprint))")
      (expect 'cljr--call-middleware-suggest-libspec
              :to-have-been-called-with "pprint" '("cljc" "clj"))))

  (it "requires the ClojureScript namespace in a :cljs branch"
    (cljr-test-with-cljc-file "(ns cljr.core)

#?(:cljs (pprint|))"
      (cljr-test-type "/pprint")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core
  (:require [cljs.pprint :as pprint]))

#?(:cljs (pprint/pprint))")
      (expect 'cljr--call-middleware-suggest-libspec
              :to-have-been-called-with "pprint" '("cljc" "cljs"))))

  (it "requires it in a splicing reader conditional too"
    (cljr-test-with-cljc-file "(ns cljr.core)

(list #?@(:cljs [(pprint|)]))"
      (cljr-test-type "/pprint")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core
  (:require [cljs.pprint :as pprint]))

(list #?@(:cljs [(pprint/pprint)]))")))

  (it "asks which namespace to require outside of a reader conditional"
    (cljr-test-with-cljc-file "(ns cljr.core)

(pprint|)"
      (cljr-test-type "/[cljs.pprint :as pprint]\rpprint")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core
  (:require [cljs.pprint :as pprint]))

(pprint/pprint)")
      (expect 'cljr--call-middleware-suggest-libspec
              :to-have-been-called-with "pprint" '("cljc" nil)))))

;;; magic-requires-cljc-test.el ends here
