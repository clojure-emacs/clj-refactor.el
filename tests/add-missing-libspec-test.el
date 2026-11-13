;;; add-missing-libspec-test.el --- Specs for cljr-add-missing-libspec -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-candidate (name type &rest more)
  "Return a candidate of the resolve-missing op: NAME, of TYPE.
MORE is further keys and values of it."
  (apply #'cljr-test-hash :name name :type type more))

(defun cljr-test-resolve-missing (&rest candidates)
  "Have the middleware find CANDIDATES for the symbol at point."
  (spy-on 'cljr--call-middleware-to-resolve-missing
          :and-return-value candidates))

(describe "cljr-add-missing-libspec"
  (before-each
    (cljr-test-without-middleware))

  (it "imports a class"
    (cljr-test-resolve-missing (cljr-test-candidate 'java.util.Date :class))
    (expect (cljr-test-refactor "(ns example.core)

(Da|te.)"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:import java.util.Date))

(Date.)"))

  (it "refers a var used without a prefix"
    (cljr-test-resolve-missing (cljr-test-candidate 'clojure.string :ns))
    (expect (cljr-test-refactor "(ns example.core)

(spl|it \"a b\" #\" \")"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:require [clojure.string :refer [split]]))

(split \"a b\" #\" \")"))

  (it "aliases the namespace to the prefix of the symbol"
    (cljr-test-resolve-missing (cljr-test-candidate 'clojure.string :ns))
    (cljr-test-with-buffer "(ns example.core)

(str/spl|it \"a b\" #\" \")"
      (cljr-add-missing-libspec)
      (expect (cljr-test-buffer-string t) :to-equal "(ns example.core
  (:require [clojure.string :as str]))

(str/spl|it \"a b\" #\" \")")
      (expect 'cljr--call-middleware-to-resolve-missing
              :to-have-been-called-with "str/split" "example.core")))

  (it "requires the namespace of a record before importing it"
    (cljr-test-resolve-missing
     (cljr-test-candidate 'modular.ring.WebrequestHandler :type))
    (expect (cljr-test-refactor "(ns example.core)

(->Webrequest|Handler)"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:require modular.ring)
  (:import modular.ring.WebrequestHandler))

(->WebrequestHandler)"))

  (it "requires the namespace of a record with dashes for its underscores"
    (cljr-test-resolve-missing
     (cljr-test-candidate 'my_app.handlers.Handler :type))
    (expect (cljr-test-refactor "(ns example.core)

(Handl|er.)"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:require my-app.handlers)
  (:import my_app.handlers.Handler))

(Handler.)"))

  (it "doesn't alias a namespace the symbol spells out in full"
    (cljr-test-resolve-missing (cljr-test-candidate 'schema.test :ns))
    (expect (cljr-test-refactor "(ns example.core)

(schema.test/validate-sch|emas)"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:require schema.test))

(schema.test/validate-schemas)"))

  (it "refers a capitalized var, which is no class"
    (cljr-test-resolve-missing (cljr-test-candidate 'my.schemas :ns))
    (expect (cljr-test-refactor "(ns example.core)

(s/validate Custom|erId 1)"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:require [my.schemas :refer [CustomerId]]))

(s/validate CustomerId 1)"))

  (it "adds to the :require form that's there"
    (cljr-test-resolve-missing (cljr-test-candidate 'clojure.string :ns))
    (expect (cljr-test-refactor "(ns example.core
  (:require [clojure.set :as set]))

(str/spl|it \"a b\" #\" \")"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(str/split \"a b\" #\" \")"))

  (it "asks which of several candidates to add"
    (cljr-test-resolve-missing (cljr-test-candidate 'java.util.Date :class)
                               (cljr-test-candidate 'java.sql.Date :class))
    (spy-on 'completing-read :and-return-value "java.sql.Date")
    (expect (cljr-test-refactor "(ns example.core)

(Da|te.)"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:import java.sql.Date))

(Date.)")
    (expect (nth 1 (spy-calls-args-for 'completing-read 0))
            :to-equal '("java.util.Date" "java.sql.Date")))

  (it "passes over the candidates the namespace has interned already"
    (cljr-test-resolve-missing
     (cljr-test-candidate 'clojure.core :ns :already-interned t)
     (cljr-test-candidate 'clojure.string :ns))
    (expect (cljr-test-refactor "(ns example.core)

(repl|ace \"a\" \"a\" \"b\")"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core
  (:require [clojure.string :refer [replace]]))

(replace \"a\" \"a\" \"b\")"))

  (it "fails when the namespace has interned every candidate"
    (cljr-test-resolve-missing
     (cljr-test-candidate 'clojure.core :ns :already-interned t))
    (cljr-test-with-buffer "(ns example.core)

(ma|p inc [1])"
      (expect (cljr-add-missing-libspec) :to-throw 'error)
      (expect (cljr-test-buffer-string) :to-equal "(ns example.core)

(map inc [1])")))

  (it "changes nothing when the middleware finds no candidate"
    (cljr-test-resolve-missing)
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value nil)
    (expect (cljr-test-refactor "(ns example.core)

(no-such-|fn 1)"
              (cljr-add-missing-libspec))
            :to-equal "(ns example.core)

(no-such-fn 1)"))

  (it "is bound to am under the prefix"
    (cljr-test-resolve-missing (cljr-test-candidate 'clojure.string :ns))
    (expect (cljr-test-refactor "(ns example.core)

(str/spl|it \"a b\" #\" \")"
              (cljr-test-keys "C-! am"))
            :to-equal "(ns example.core
  (:require [clojure.string :as str]))

(str/split \"a b\" #\" \")")))

;;; add-missing-libspec-test.el ends here
