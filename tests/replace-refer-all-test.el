;;; replace-refer-all-test.el --- Specs for replacing :refer :all with an alias -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defconst cljr-test-refer-all-file "src/example/one.clj"
  "The file of the test project that refers to all of example.two.")

(defun cljr-test-used-publics ()
  "Return what find-used-publics answers with for example.two in the test file.
That's a string of EDN."
  (let ((file (cljr-test-file cljr-test-refer-all-file)))
    (format "({:line-beg 5 :line-end 5 :col-beg 13 :col-end 30 :file %S :name \"foo\"}
 {:line-beg 8 :line-end 8 :col-beg 15 :col-end 28 :file %S :name \"star*\"})"
            file file)))

(defmacro cljr-test-with-refer-all-file (content &rest body)
  "Evaluate BODY in a file holding CONTENT; return its text.
The middleware answers with `cljr-test-used-publics'."
  (declare (indent 1) (debug t))
  `(cljr-test-refactor-file cljr-test-refer-all-file ,content
     (cljr-test-middleware-answers (cljr-test-used-publics))
     ,@body))

(describe "cljr-stop-referring, for a namespace that's referred to with :all"
  (before-each
    (cljr-test-without-middleware))

  (it "asks for an alias and puts it on the vars of the namespace"
    (expect (cljr-test-with-refer-all-file "(ns example.one
  (:require [|example.two :refer :all]))

(defn bar []
  (str \"bar\" (foo) \"goo\"))

(defn sky []
  (str \"sky: \" (star*)))"
              (cljr-test-call #'cljr-stop-referring "two"))
            :to-equal "(ns example.one
  (:require [example.two :as two]))

(defn bar []
  (str \"bar\" (two/foo) \"goo\"))

(defn sky []
  (str \"sky: \" (two/star*)))
")
    (expect (cljr-test-middleware-request "used-ns") :to-equal "example.two")
    (expect (cadr (spy-calls-args-for 'cljr--call-middleware-sync 0))
            :to-equal "used-publics"))

  (it "uses the alias the namespace already has"
    (expect (cljr-test-with-refer-all-file "(ns example.one
  (:require [|example.two :as two :refer :all]))

(defn bar []
  (str \"bar\" (foo) \"goo\"))

(defn sky []
  (str \"sky: \" (star*)))"
              (cljr-stop-referring))
            :to-equal "(ns example.one
  (:require [example.two :as two]))

(defn bar []
  (str \"bar\" (two/foo) \"goo\"))

(defn sky []
  (str \"sky: \" (two/star*)))
"))

  (it "leaves point where it was"
    (cljr-test-with-refer-all-file "(ns example.one
  (:require [example.|two :refer :all]))

(defn bar []
  (str \"bar\" (foo) \"goo\"))

(defn sky []
  (str \"sky: \" (star*)))"
      (cljr-test-call #'cljr-stop-referring "two")
      (expect (cljr-test-buffer-string t) :to-equal "(ns example.one
  (:require [example.|two :as two]))

(defn bar []
  (str \"bar\" (two/foo) \"goo\"))

(defn sky []
  (str \"sky: \" (two/star*)))
"))))

(describe "cljr--replace-refer-all-with-alias"
  (it "only touches the libspec of the namespace it's given"
    (expect (cljr-test-with-refer-all-file "(ns example.one
  (:require [example.three :refer :all]
            [example.two :refer :all]))

(defn bar []
  (str \"bar\" (foo) \"goo\"))"
              (goto-char (point-min))
              (cljr--replace-refer-all-with-alias
               "example.two"
               (list (cljr-test-occurrence cljr-test-refer-all-file
                                           :line-beg 6 :line-end 6
                                           :col-beg 13 :col-end 30
                                           :name "foo"))
               "two"))
            :to-equal "(ns example.one
  (:require [example.three :refer :all]
            [example.two :as two]))

(defn bar []
  (str \"bar\" (two/foo) \"goo\"))
")))

;;; replace-refer-all-test.el ends here
