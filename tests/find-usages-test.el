;;; find-usages-test.el --- Specs for finding the usages of a symbol -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-usage (file line-beg line-end col-beg col-end match)
  "Return a message of the find-symbol op with an occurrence of foo.
It's in FILE, relative to the test project, from LINE-BEG and COL-BEG
to LINE-END and COL-END, and MATCH is the form it's in."
  (nrepl-dict "occurrence"
              (format "{:line-beg %d :line-end %d :col-beg %d :col-end %d :name \"foo\" :file %S :match %S}"
                      line-beg line-end col-beg col-end (cljr-test-file file) match)))

(defun cljr-test-usage-in-two ()
  (cljr-test-usage "src/example/two.clj" 3 4 7 9 "(defn foo []\n  \"goo\")"))

(defun cljr-test-usage-in-one ()
  (cljr-test-usage "src/example/one.clj" 5 5 15 23 "(str \"bar\" (two/foo) \"goo\"))"))

(defun cljr-test-find-symbol-answers (messages)
  "Have the find-symbol op answer with MESSAGES in this spec.
MESSAGES is a function, called once the test project is there."
  (spy-on 'cljr--find-symbol :and-call-fake
          (lambda (_symbol _ns callback)
            ;; The messages of a search are counted in the buffer of the
            ;; REPL, which starts counting anew with each search.
            (with-temp-buffer
              (mapc callback (funcall messages))))))

(defmacro cljr-test-with-usages-project (&rest body)
  "Evaluate BODY in example.one, with point on a use of example.two/foo."
  (declare (indent 0) (debug t))
  `(cljr-test-with-example-project
     (cljr-test-goto "two/foo")
     ,@body))

(defun cljr-test-usages (&optional with-point)
  "Return the text of the buffer listing the usages.
With WITH-POINT non-nil, mark where point is with a `|'."
  (with-current-buffer cljr--find-symbol-buffer
    (cljr-test-buffer-string with-point)))

(describe "cljr-find-usages"
  (before-each
    (cljr-test-without-middleware)
    (spy-on 'cider-spinner-start)
    (spy-on 'message)
    (spy-on 'cljr--var-info :and-return-value
            (nrepl-dict "ns" "example.two" "name" "foo")))

  (after-each
    (when (get-buffer cljr--find-symbol-buffer)
      (kill-buffer cljr--find-symbol-buffer)))

  (it "lists where the symbol at point occurs in the project"
    (cljr-test-find-symbol-answers
     (lambda ()
       (list (cljr-test-usage-in-two)
             (cljr-test-usage-in-one)
             (nrepl-dict "count" 2))))
    (cljr-test-with-usages-project
      (cljr-find-usages)
      (expect (cljr-test-usages t)
              :to-equal "'foo' occurs in the following places:

|src/example/two.clj:3:6: (defn foo []
src/example/one.clj:5:14: (str \"bar\" (two/foo) \"goo\"))

Find symbol finished: 2 occurrences found"))
    (expect (seq-take (spy-calls-args-for 'cljr--find-symbol 0) 2)
            :to-equal '("foo" "example.two")))

  (it "lists an occurrence that the middleware reports twice only once"
    (cljr-test-find-symbol-answers
     (lambda ()
       (list (cljr-test-usage-in-two)
             (cljr-test-usage-in-two)
             (cljr-test-usage-in-one)
             (nrepl-dict "count" 3))))
    (cljr-test-with-usages-project
      (cljr-find-usages)
      (expect (cljr-test-usages)
              :to-equal "'foo' occurs in the following places:

src/example/two.clj:3:6: (defn foo []
src/example/one.clj:5:14: (str \"bar\" (two/foo) \"goo\"))

Find symbol finished: 3 occurrences found")))

  (it "doesn't say occurrences when there's just one"
    (cljr-test-find-symbol-answers
     (lambda ()
       (list (cljr-test-usage-in-two)
             (nrepl-dict "count" 1))))
    (cljr-test-with-usages-project
      (cljr-find-usages)
      (expect (cljr-test-usages)
              :to-equal "'foo' occurs in the following places:

src/example/two.clj:3:6: (defn foo []

Find symbol finished: 1 occurrence found")))

  (it "lists the occurrences as they come, before the search is over"
    (cljr-test-find-symbol-answers
     (lambda ()
       (list (cljr-test-usage-in-two))))
    (cljr-test-with-usages-project
      ;; Nothing ends this search, and its timeout shouldn't outlive the spec.
      (let ((cljr-middleware-timeout nil))
        (cljr-find-usages))
      (expect (cljr-test-usages)
              :to-equal "'foo' occurs in the following places:

src/example/two.clj:3:6: (defn foo []
")))

  (it "starts over with each search"
    (cljr-test-find-symbol-answers
     (lambda ()
       (list (cljr-test-usage-in-two)
             (cljr-test-usage-in-one)
             (nrepl-dict "count" 2))))
    (cljr-test-with-usages-project
      (cljr-find-usages)
      (cljr-find-usages)
      (expect (cljr-test-usages)
              :to-equal "'foo' occurs in the following places:

src/example/two.clj:3:6: (defn foo []
src/example/one.clj:5:14: (str \"bar\" (two/foo) \"goo\"))

Find symbol finished: 2 occurrences found")))

  (it "takes you from an occurrence in the list to the place it's at"
    (cljr-test-find-symbol-answers
     (lambda ()
       (list (cljr-test-usage-in-two)
             (cljr-test-usage-in-one)
             (nrepl-dict "count" 2))))
    (cljr-test-with-usages-project
      (cljr-find-usages)
      (expect (window-buffer) :to-be (get-buffer cljr--find-symbol-buffer))
      (set-buffer (window-buffer))
      (expect major-mode :to-be 'grep-mode)
      (compile-goto-error)
      (set-buffer (window-buffer))
      (expect (buffer-file-name) :to-equal (cljr-test-file "src/example/two.clj"))
      (expect (cljr-test-buffer-string t) :to-equal "(ns example.two)

(defn |foo []
  \"goo\")

(defn star* []
  \"star\")
")))

  (it "saves the buffer first, for the middleware to see what's in it"
    (cljr-test-find-symbol-answers
     (lambda ()
       (list (nrepl-dict "count" 0))))
    (cljr-test-with-usages-project
      (insert "two/foo ")
      (cljr-find-usages)
      (expect (buffer-modified-p) :to-be nil)))

  (it "needs a symbol at point"
    (spy-on 'cljr--find-symbol)
    (cljr-test-with-usages-project
      (goto-char (point-max))
      (expect (cljr-find-usages) :to-throw 'user-error)
      (expect 'cljr--find-symbol :not :to-have-been-called)
      (expect (get-buffer cljr--find-symbol-buffer) :to-be nil))))

;;; find-usages-test.el ends here
