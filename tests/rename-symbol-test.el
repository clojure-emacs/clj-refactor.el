;;; rename-symbol-test.el --- Specs for renaming symbols -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-rename-occurrence (file &rest keys-and-values)
  "Return the middleware's map of KEYS-AND-VALUES for an occurrence in FILE.
FILE is relative to the test project."
  (apply #'cljr-test-occurrence file :match "" keys-and-values))

(defun cljr-test-occurrences-of-foo ()
  "Return what find-symbol answers with for example.two/foo."
  (list (cljr-test-rename-occurrence "src/example/two.clj"
                                     :line-beg 3 :line-end 4 :col-beg 7 :col-end 9
                                     :name "foo")
        (cljr-test-rename-occurrence "src/example/one.clj"
                                     :line-beg 5 :line-end 5 :col-beg 15 :col-end 23
                                     :name "foo")))

(defun cljr-test-occurrences-of-star ()
  "Return what find-symbol answers with for example.two/star*."
  (list (cljr-test-rename-occurrence "src/example/two.clj"
                                     :line-beg 6 :line-end 7 :col-beg 7 :col-end 10
                                     :name "star*")
        (cljr-test-rename-occurrence "src/example/one.clj"
                                     :line-beg 8 :line-end 8 :col-beg 17 :col-end 27
                                     :name "star*")))

(defun cljr-test-find-occurrences (occurrences)
  "Have the search for a symbol come up with OCCURRENCES in this spec.
OCCURRENCES is a function, called once the test project is there."
  (spy-on 'cljr--find-symbol-occurrences :and-call-fake
          (lambda (_symbol _ns callback)
            (funcall callback (funcall occurrences)))))

(defmacro cljr-test-with-rename-project (&rest body)
  "Evaluate BODY in example.one, in a project that has example.two as well.
The messages the renaming leaves for after the command stay in BODY."
  (declare (indent 0) (debug t))
  `(cljr-test-with-example-project
     (let ((cljr--post-command-messages nil))
       ,@body)))

(describe "cljr-rename-symbol"
  (before-each
    (cljr-test-without-middleware)
    (spy-on 'cljr--warm-ast-cache)
    (spy-on 'message))

  (it "renames the definition of a symbol and its uses in other files"
    (cljr-test-find-occurrences #'cljr-test-occurrences-of-foo)
    (spy-on 'cljr--var-info :and-return-value
            (nrepl-dict "ns" "example.two" "name" "foo"))
    (cljr-test-with-rename-project
      (cljr-test-goto "two/foo")
      (cljr-rename-symbol "baz")
      (expect (cljr-test-file-string "src/example/two.clj")
              :to-equal "(ns example.two)

(defn baz []
  \"goo\")

(defn star* []
  \"star\")
")
      (expect (cljr-test-file-string "src/example/one.clj")
              :to-equal "(ns example.one
  (:require [example.two :as two]))

(defn bar []
  (str \"bar\" (two/baz) \"goo\"))

(defn sky []
  (str \"sky: \" (two/star*)))
")
      (expect cljr--post-command-messages
              :to-equal '("Renamed 2 occurrences of foo")))
    (expect (seq-take (spy-calls-args-for 'cljr--find-symbol-occurrences 0) 2)
            :to-equal '("foo" "example.two"))
    (expect 'cljr--warm-ast-cache :to-have-been-called-times 1))

  (it "renames a symbol whose name has a character that's special in a regexp"
    (cljr-test-find-occurrences #'cljr-test-occurrences-of-star)
    (spy-on 'cljr--var-info :and-return-value
            (nrepl-dict "ns" "example.two" "name" "star*"))
    (cljr-test-with-rename-project
      (cljr-test-goto "two/star*")
      (cljr-rename-symbol "asterisk*")
      (expect (cljr-test-file-string "src/example/two.clj")
              :to-equal "(ns example.two)

(defn foo []
  \"goo\")

(defn asterisk* []
  \"star\")
")
      (expect (cljr-test-file-string "src/example/one.clj")
              :to-equal "(ns example.one
  (:require [example.two :as two]))

(defn bar []
  (str \"bar\" (two/foo) \"goo\"))

(defn sky []
  (str \"sky: \" (two/asterisk*)))
")))

  (it "saves the files it changes and closes those it had to open"
    (cljr-test-find-occurrences #'cljr-test-occurrences-of-foo)
    (spy-on 'cljr--var-info :and-return-value
            (nrepl-dict "ns" "example.two" "name" "foo"))
    (cljr-test-with-rename-project
      (cljr-test-goto "two/foo")
      (cljr-rename-symbol "baz")
      (expect (buffer-file-name) :to-equal (cljr-test-file "src/example/one.clj"))
      (expect (buffer-modified-p) :to-be nil)
      (expect (get-file-buffer (cljr-test-file "src/example/two.clj")) :to-be nil)
      (expect (cljr-test-file-string "src/example/one.clj" 'on-disk)
              :to-match "(two/baz)")))

  (it "asks for the new name, starting from the current one"
    (cljr-test-find-occurrences #'cljr-test-occurrences-of-foo)
    (spy-on 'cljr--var-info :and-return-value
            (nrepl-dict "ns" "example.two" "name" "foo"))
    (cljr-test-with-rename-project
      (cljr-test-goto "two/foo")
      (cljr-test-call #'cljr-rename-symbol "-bar")
      (expect (cljr-test-file-string "src/example/two.clj")
              :to-match "(defn foo-bar \\[\\]")
      (expect (cljr-test-file-string "src/example/one.clj")
              :to-match "(two/foo-bar)")))

  (it "shows what it's about to change and asks before changing it"
    (cljr-test-find-occurrences #'cljr-test-occurrences-of-foo)
    (spy-on 'cljr--var-info :and-return-value
            (nrepl-dict "ns" "example.two" "name" "foo"))
    (spy-on 'y-or-n-p :and-return-value nil)
    (cljr-test-with-rename-project
      (cljr-test-goto "two/foo")
      (let ((cljr-preview-refactorings t))
        (cljr-rename-symbol "baz"))
      (expect 'y-or-n-p :to-have-been-called-with
              "Apply Rename 2 occurrence(s) to 2 file(s)? ")
      (expect (cljr-test-file-string "src/example/two.clj")
              :to-equal cljr-test-example-two)
      (expect (cljr-test-file-string "src/example/one.clj")
              :to-equal cljr-test-example-one)
      (expect cljr--post-command-messages :to-be nil))
    (expect 'cljr--warm-ast-cache :not :to-have-been-called))

  (it "refuses to run in a narrowed buffer"
    (spy-on 'cljr--find-symbol-occurrences)
    (cljr-test-with-rename-project
      (cljr-test-goto "two/foo")
      (narrow-to-defun)
      (expect (cljr-rename-symbol "baz") :to-throw 'error)
      (expect 'cljr--find-symbol-occurrences :not :to-have-been-called))))

(describe "cljr--rename-occurrences"
  (it "takes a function that makes the new name out of the old one"
    (cljr-test-with-rename-project
      (cljr--rename-occurrences
       (append (cljr-test-occurrences-of-foo) (cljr-test-occurrences-of-star))
       #'upcase)
      (expect (cljr-test-file-string "src/example/two.clj")
              :to-equal "(ns example.two)

(defn FOO []
  \"goo\")

(defn STAR* []
  \"star\")
")
      (expect (cljr-test-file-string "src/example/one.clj")
              :to-equal "(ns example.one
  (:require [example.two :as two]))

(defn bar []
  (str \"bar\" (two/FOO) \"goo\"))

(defn sky []
  (str \"sky: \" (two/STAR*)))
")))

  (it "leaves an occurrence alone when the symbol isn't where the middleware says"
    (cljr-test-with-rename-project
      (cljr--rename-occurrences
       (list (cljr-test-rename-occurrence "src/example/two.clj"
                                          :line-beg 1 :line-end 1 :col-beg 1 :col-end 17
                                          :name "foo"))
       "baz")
      (expect (cljr-test-file-string "src/example/two.clj")
              :to-equal cljr-test-example-two))))

;;; rename-symbol-test.el ends here
