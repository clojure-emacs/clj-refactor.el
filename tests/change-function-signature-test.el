;;; change-function-signature-test.el --- Specs for changing a function's signature -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

;;; What the middleware and the edit buffer would hand over

(defun cljr-test-swap-foo-and-bar ()
  (list (cljr-test-param-keep 0 1 "foo")
        (cljr-test-param-keep 1 0 "bar")
        (cljr-test-param-keep 2 2 "baz")))

(defun cljr-test-swap-bar-and-baz ()
  (list (cljr-test-param-keep 0 0 "foo")
        (cljr-test-param-keep 1 2 "bar")
        (cljr-test-param-keep 2 1 "baz")))

(defun cljr-test-rename-baz-to-qux ()
  (list (cljr-test-param-keep 0 0 "foo")
        (cljr-test-param-keep 1 1 "bar")
        (cljr-test-param-keep 2 2 "baz" "qux")))

(defun cljr-test-add-qux ()
  (list (cljr-test-param-keep 0 0 "foo")
        (cljr-test-param-keep 1 1 "bar")
        (cljr-test-param-keep 2 2 "baz")
        (cljr-test-param-add 3 "qux")))

(defun cljr-test-remove-baz ()
  (list (cljr-test-param-keep 0 0 "foo")
        (cljr-test-param-keep 1 1 "bar")
        (cljr-test-param-remove 2 "baz")))

(defconst cljr-test-signature-file "src/example/core.clj"
  "The file of the test project that the occurrences of tt are in.")

(defun cljr-test-tt-occurrence (line col-beg col-end match)
  "Return an occurrence of core/tt, the way find-symbol reports it.
It's on LINE of `cljr-test-signature-file', between COL-BEG and COL-END,
and MATCH is the form it's in."
  (cljr-test-occurrence cljr-test-signature-file
                        :line-beg line :line-end line
                        :col-beg col-beg :col-end col-end
                        :name "core/tt" :match match))

(defun cljr-test-review-lines ()
  "Return the lines of the buffer listing what's left to do by hand.
The file names in them are relative to the test project."
  (when-let* ((buffer (get-buffer cljr--manual-intervention-buffer)))
    (with-current-buffer buffer
      (mapcar (lambda (line)
                (string-remove-prefix cljr-test-project-root line))
              (split-string (cljr-test-buffer-string) "\n" t)))))

(defun cljr-test-change-signature (content changes &rest occurrences)
  "Apply CHANGES to OCCURRENCES of tt in a file holding CONTENT.
Each occurrence is the list of arguments of `cljr-test-tt-occurrence'.
Return a list: the text of the file afterwards, followed by the lines
of the buffer listing what's left to do by hand."
  (cljr-test-with-project ()
    (cljr-test-visit cljr-test-signature-file content)
    (cljr--change-function-signature
     (mapcar (lambda (occurrence) (apply #'cljr-test-tt-occurrence occurrence))
             occurrences)
     changes)
    (cons (cljr-test-file-string cljr-test-signature-file)
          (cljr-test-review-lines))))

(defconst cljr-test-review-header
  "The following occurrence(s) couldn't be handled automatically:")

(describe "cljr--change-function-signature"
  ;; It only ever adds to the buffer listing what's left to do by hand;
  ;; it's `cljr-change-function-signature' that starts a new one.
  (after-each
    (when (get-buffer cljr--manual-intervention-buffer)
      (kill-buffer cljr--manual-intervention-buffer)))

  (describe "in the definition"
    (it "reorders the parameters"
      (expect (cljr-test-change-signature "(ns core)

(defn tt [foo bar baz]
  (println foo bar baz))"
                (cljr-test-swap-foo-and-bar)
                '(3 1 25 "(defn tt [foo bar baz]\n  (println foo bar baz))"))
              :to-equal '("(ns core)

(defn tt [bar foo baz]
  (println foo bar baz))
")))

    (it "moves the schema of a parameter along with it"
      (expect (cljr-test-change-signature "(ns core)

(defn tt [foo :- s/str bar :- s/int baz :- User]
  (println foo bar baz))"
                (cljr-test-swap-foo-and-bar)
                '(3 1 25 "(defn tt [foo :- s/str bar :- s/int baz :- User]\n  (println foo bar baz))"))
              :to-equal '("(ns core)

(defn tt [bar :- s/int foo :- s/str baz :- User]
  (println foo bar baz))
")))

    (it "puts the parameters on a line of their own when they end up too far to the right"
      (expect (cljr-test-change-signature "(ns core)

(defn this-function-has-a-super-long-name-which-warrants-putting-the-lambda-list-on-its-own-line [foo bar baz]
  (println foo bar baz))"
                (cljr-test-swap-foo-and-bar)
                '(3 1 25 "(defn this-function-has-a-super-long-name-which-warrants-putting-the-lambda-list-on-its-own-line [foo bar baz]\n  (println foo bar baz))"))
              :to-equal '("(ns core)

(defn this-function-has-a-super-long-name-which-warrants-putting-the-lambda-list-on-its-own-line
  [bar foo baz]
  (println foo bar baz))
")))

    (it "renames a parameter with cljr-rename-symbol"
      ;; Renaming for real takes a REPL: this is what it would do to the
      ;; parameter point is on.
      (spy-on 'cljr-rename-symbol :and-call-fake
              (lambda (new-name)
                (let ((old-name (cider-symbol-at-point))
                      (end (save-excursion (cljr--goto-toplevel)
                                           (paredit-forward)
                                           (point-marker))))
                  (save-excursion
                    (cljr--goto-toplevel)
                    (while (re-search-forward (format "\\_<%s\\_>" old-name) end t)
                      (replace-match new-name t t))))))
      (expect (cljr-test-change-signature "(ns core)

(defn tt [foo bar baz]
  (println foo bar baz))"
                (cljr-test-rename-baz-to-qux)
                '(3 1 25 "(defn tt [foo bar baz]\n  (println foo bar baz))"))
              :to-equal '("(ns core)

(defn tt [foo bar qux]
  (println foo bar qux))
"))
      (expect 'cljr-rename-symbol :to-have-been-called-times 1)
      (expect 'cljr-rename-symbol :to-have-been-called-with "qux"))

    (it "adds a parameter"
      (expect (cljr-test-change-signature "(ns core)

(defn tt [foo bar baz]
  (println foo bar baz))"
                (cljr-test-add-qux)
                '(3 1 25 "(defn tt [foo bar baz]\n  (println foo bar baz))"))
              :to-equal '("(ns core)

(defn tt [foo bar baz qux]
  (println foo bar baz))
")))

    (it "removes a parameter, but not its uses in the body"
      (expect (cljr-test-change-signature "(ns core)

(defn tt [foo bar baz]
  (println foo bar baz))"
                (cljr-test-remove-baz)
                '(3 1 25 "(defn tt [foo bar baz]\n  (println foo bar baz))"))
              :to-equal '("(ns core)

(defn tt [foo bar]
  (println foo bar baz))
"))))

  (describe "in a call"
    (it "reorders the arguments"
      (expect (cljr-test-change-signature "(ns core)

(defn regular-call-site []
  (tt 1 2 3))"
                (cljr-test-swap-foo-and-bar)
                '(4 4 7 "(tt 1 2 3))"))
              :to-equal '("(ns core)

(defn regular-call-site []
  (tt 2 1 3))
")))

    (it "puts the default in the place of an added parameter"
      (expect (cljr-test-change-signature "(ns core)

(defn regular-call-site []
  (tt 1 2 3))"
                (cljr-test-add-qux)
                '(4 4 7 "(tt 1 2 3))"))
              :to-equal '("(ns core)

(defn regular-call-site []
  (tt 1 2 3 nil))
")))

    (it "leaves the argument of a removed parameter for the user to deal with"
      (expect (cljr-test-change-signature "(ns core)

(defn regular-call-site []
  (tt 1 2 3))"
                (cljr-test-remove-baz)
                '(4 4 7 "(tt 1 2 3))"))
              :to-equal (list "(ns core)

(defn regular-call-site []
  (tt 1 2 3))
"
                              cljr-test-review-header
                              "src/example/core.clj:4: (tt 1 2 3))")))

    (it "is still a call when it's on the line of a defn"
      (expect (cljr-test-change-signature "(ns core)

(defn wrapper [x] (tt x 2 3))"
                (cljr-test-swap-foo-and-bar)
                '(3 20 22 "(defn wrapper [x] (tt x 2 3))"))
              :to-equal '("(ns core)

(defn wrapper [x] (tt 2 x 3))
")))

    (it "is left alone when it's a call to another arity"
      (expect (cljr-test-change-signature "(ns core)

(defn caller []
  [(tt 1) (tt 1 2 3)])"
                (cljr-test-swap-foo-and-bar)
                '(4 5 7 "(tt 1)")
                '(4 12 14 "(tt 1 2 3)"))
              :to-equal '("(ns core)

(defn caller []
  [(tt 1) (tt 2 1 3)])
")))

    (it "is left for the user to deal with when the function takes rest arguments"
      (expect (cljr-test-change-signature "(ns core)

(defn regular-call-site []
  (tt 1 2 3))"
                (list (cljr-test-param-keep 0 1 "foo")
                      (cljr-test-param-keep 1 0 "&")
                      (cljr-test-param-keep 2 2 "more"))
                '(4 4 7 "(tt 1 2 3))"))
              :to-equal (list "(ns core)

(defn regular-call-site []
  (tt 1 2 3))
"
                              cljr-test-review-header
                              "src/example/core.clj:4: (tt 1 2 3))"))))

  (describe "where the function is passed around"
    (it "leaves a higher-order use for the user to deal with"
      (expect (cljr-test-change-signature "(ns core)

(defn higher-order-call-site []
  (map tt [1] [2] [3]))"
                (cljr-test-swap-foo-and-bar)
                '(4 8 11 "(map tt [1] [2] [3]))"))
              :to-equal (list "(ns core)

(defn higher-order-call-site []
  (map tt [1] [2] [3]))
"
                              cljr-test-review-header
                              "src/example/core.clj:4: (map tt [1] [2] [3]))")))

    (it "reorders the arguments of a partial that has all those that move"
      (expect (cljr-test-change-signature "(ns core)

(defn partial-call-site []
  (map (partial tt 1 :two) 'three))"
                (cljr-test-swap-foo-and-bar)
                '(4 17 20 "(map (partial tt [1]) [2] [3]))"))
              :to-equal '("(ns core)

(defn partial-call-site []
  (map (partial tt :two 1) 'three))
")))

    (it "leaves a partial that lacks some of them for the user to deal with"
      (expect (cljr-test-change-signature "(ns core)

(defn partial-call-site []
  (map (partial tt 1) 2 3))"
                (cljr-test-swap-foo-and-bar)
                '(4 17 20 "(map (partial tt [1]) [2] [3]))"))
              :to-equal (list "(ns core)

(defn partial-call-site []
  (map (partial tt 1) 2 3))
"
                              cljr-test-review-header
                              "src/example/core.clj:4: (map (partial tt 1) 2 3))")))

    (it "leaves a partial for the user to deal with when a parameter is added"
      (expect (cljr-test-change-signature "(ns core)

(defn partial-call-site []
  (map (partial tt 1 :two) 'three))"
                (cljr-test-add-qux)
                '(4 17 20 "(map (partial tt [1]) [2] [3]))"))
              :to-equal (list "(ns core)

(defn partial-call-site []
  (map (partial tt 1 :two) 'three))
"
                              cljr-test-review-header
                              "src/example/core.clj:4: (map (partial tt 1 :two) 'three))")))

    (it "reorders the arguments of an apply that are spelled out"
      (expect (cljr-test-change-signature "(ns core)

(defn apply-call-site []
  (let [args [1]]
    (apply tt [1] [2] args)))"
                (cljr-test-swap-foo-and-bar)
                '(5 12 15 "(apply tt [1] [2] args)))"))
              :to-equal '("(ns core)

(defn apply-call-site []
  (let [args [1]]
    (apply tt [2] [1] args)))
")))

    (it "leaves an apply for the user to deal with when the change reaches its last argument"
      (expect (cljr-test-change-signature "(ns core)

(defn apply-call-site []
  (let [args [1]]
    (apply tt [1] [2] args)))"
                (cljr-test-swap-bar-and-baz)
                '(5 12 15 "(apply tt [1] [2] args)))"))
              :to-equal (list "(ns core)

(defn apply-call-site []
  (let [args [1]]
    (apply tt [1] [2] args)))
"
                              cljr-test-review-header
                              "src/example/core.clj:5: (apply tt [1] [2] args)))"))))

  (describe "across the occurrences of a function"
    (it "changes the definition and its calls in one go"
      (expect (cljr-test-change-signature "(ns core)

(defn tt [foo bar baz]
  (println foo bar baz))

(defn caller []
  (tt 1 2 3)
  (map tt [1] [2] [3]))"
                (cljr-test-swap-foo-and-bar)
                '(3 1 25 "(defn tt [foo bar baz]\n  (println foo bar baz))")
                '(7 4 6 "(tt 1 2 3)")
                '(8 8 10 "(map tt [1] [2] [3]))"))
              :to-equal (list "(ns core)

(defn tt [bar foo baz]
  (println foo bar baz))

(defn caller []
  (tt 2 1 3)
  (map tt [1] [2] [3]))
"
                              cljr-test-review-header
                              "src/example/core.clj:8: (map tt [1] [2] [3]))")))

    (it "skips the occurrences in the ns form"
      (expect (cljr-test-change-signature "(ns core
  (:require [other :refer [tt]]))

(tt 1 2 3)"
                (cljr-test-swap-foo-and-bar)
                '(2 28 30 "(:require [other :refer [tt]]))")
                '(4 2 4 "(tt 1 2 3)"))
              :to-equal '("(ns core
  (:require [other :refer [tt]]))

(tt 2 1 3)
")))

    (it "saves the files it changes"
      (cljr-test-with-project ()
        (cljr-test-visit cljr-test-signature-file "(ns core)

(tt 1 2 3)")
        (cljr--change-function-signature
         (list (cljr-test-tt-occurrence 3 2 4 "(tt 1 2 3)"))
         (cljr-test-swap-foo-and-bar))
        (expect (buffer-modified-p) :to-be nil)
        (expect (cljr-test-file-string cljr-test-signature-file 'on-disk)
                :to-equal "(ns core)

(tt 2 1 3)
")))

    (it "shows what's left to do by hand in a grep buffer"
      (cljr-test-with-project ()
        (cljr-test-visit cljr-test-signature-file "(ns core)

(map tt [1] [2] [3])")
        (cljr--change-function-signature
         (list (cljr-test-tt-occurrence 3 6 8 "(map tt [1] [2] [3])"))
         (cljr-test-swap-foo-and-bar))
        (expect (buffer-name) :to-equal cljr--manual-intervention-buffer)
        (expect major-mode :to-be 'grep-mode)
        (expect (cljr-test-buffer-string t)
                :to-equal (format "%s\n\n|%s:3: (map tt [1] [2] [3])\n"
                                  cljr-test-review-header
                                  (cljr-test-file cljr-test-signature-file)))))))

;;; change-function-signature-test.el ends here
