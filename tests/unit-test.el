(require 'paredit)
(require 'clj-refactor)
(require 'buttercup)

;; NOTE: please remember, without an `it` block, your tests will not be run!
;; You can learn about Buttercup's syntax here:
;; https://github.com/jorgenschaefer/emacs-buttercup/blob/v1.24/docs/running-tests.md

(describe "cljr--ns-name"
  (it "returns the ns name of its argument"
    (expect (cljr--ns-name "com.corp.foo") :to-equal "foo")
    (expect (cljr--ns-name "foo") :to-equal "foo")))

(describe "cljr--namespace-aliases"
  (it "reduces to a unique list from middleware"
    (spy-on 'cljr--call-middleware-for-namespace-aliases
            :and-return-value
            (parseedn-read-str
             "{:clj  {t (clojure.test) set (clojure.set) sut (alpha shared)}
               :cljs {t (cljs.test) set (clojure.set) sut (beta shared)}}"))
    (expect (cljr--namespace-aliases)
            :to-equal
            '((t clojure.test (:clj))
              (sut alpha (:clj))
              (t cljs.test (:cljs))
              (set clojure.set (:clj :cljs))
              (sut beta (:cljs))
              (sut shared (:clj :cljs))))))
