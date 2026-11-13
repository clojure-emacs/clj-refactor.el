;;; inline-symbol-test.el --- Specs for inlining symbols -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defconst cljr-test-inline-file "src/example/core.clj"
  "The file of the test project that the symbols are inlined in.")

(defun cljr-test-in-file (&rest keys-and-values)
  "Return the middleware's map of KEYS-AND-VALUES for a form in the test file."
  (apply #'cljr-test-occurrence cljr-test-inline-file keys-and-values))

(defun cljr-test-definition-of-answer (&optional used)
  "Return what extract-definition answers with for `(def answer 42)'.
It's on the third line of the test file, and with USED non-nil
there's an occurrence of it in an `(inc answer)' on the fifth."
  (let ((file (cljr-test-file cljr-test-inline-file)))
    (format "{:definition {:line-beg 3 :line-end 3 :col-beg 1 :col-end 16
                           :name \"example.core/answer\" :file %S
                           :match \"(def answer 42)\" :definition \"42\"}
              :occurrences (%s)}"
            file
            (if used
                (format "{:line-beg 5 :line-end 5 :col-beg 6 :col-end 12
                          :name \"example.core/answer\" :file %S
                          :match \"(inc answer)\"}"
                        file)
              ""))))

(defmacro cljr-test-inline (content definition &rest occurrences)
  "Inline DEFINITION at OCCURRENCES in a file holding CONTENT; return its text.
They are what the extract-definition op answers with, and are evaluated
with the test project in place."
  (declare (indent 1))
  `(cljr-test-refactor-file cljr-test-inline-file ,content
     (cljr--inline-symbol ,definition (list ,@occurrences))))

(describe "cljr--inline-symbol"
  (it "replaces the uses of a def with its value and removes the def"
    (expect (cljr-test-inline "(def my-constant 123)

(defn my-function []
  (let [another-val 321]
    (println my-constant my-constant another-val)))"
              (cljr-test-in-file :line-beg 1 :line-end 1 :col-beg 1 :col-end 22
                                 :name "refactor-nrepl.test/my-constant"
                                 :match "(def my-constant 123)"
                                 :definition "123")
              (cljr-test-in-file :line-beg 5 :line-end 5 :col-beg 14 :col-end 26
                                 :name "refactor-nrepl.test/my-constant"
                                 :match "(println my-constant my-constant another-val)))")
              (cljr-test-in-file :line-beg 5 :line-end 5 :col-beg 26 :col-end 38
                                 :name "refactor-nrepl.test/my-constant"
                                 :match "(println my-constant my-constant another-val)))"))
            :to-equal "(defn my-function []
  (let [another-val 321]
    (println 123 123 another-val)))
"))

  (it "removes the let when its only binding is inlined"
    (expect (cljr-test-inline "(def my-constant 123)

(defn my-function []
  (let [another-val 321]
    (println my-constant my-constant another-val)))"
              (cljr-test-in-file :line-beg 4 :line-end 4 :col-beg 9 :col-end 21
                                 :name "another-val"
                                 :match "(let [another-val 321]"
                                 :definition "321")
              (cljr-test-in-file :line-beg 5 :line-end 5 :col-beg 38 :col-end 50
                                 :name "another-val"
                                 :match "(println my-constant my-constant another-val)))"))
            :to-equal "(def my-constant 123)

(defn my-function []
  (println my-constant my-constant 321))
"))

  (it "removes an unused binding from the end of a let"
    (expect (cljr-test-inline "(def my-constant 123)

(defn my-function []
  (let [another-val 321
        some-val 110]
    (println my-constant my-constant another-val)))"
              (cljr-test-in-file :line-beg 5 :line-end 5 :col-beg 9 :col-end 17
                                 :name "some-val"
                                 :match "some-val 110]"
                                 :definition "110"))
            :to-equal "(def my-constant 123)

(defn my-function []
  (let [another-val 321]
    (println my-constant my-constant another-val)))
"))

  (it "keeps the other bindings of a let"
    (expect (cljr-test-inline "(defn my-function []
  (let [another-val 321
        some-val 110]
    (println another-val some-val)))"
              (cljr-test-in-file :line-beg 2 :line-end 2 :col-beg 9 :col-end 21
                                 :name "another-val"
                                 :match "(let [another-val 321"
                                 :definition "321")
              (cljr-test-in-file :line-beg 4 :line-end 4 :col-beg 14 :col-end 25
                                 :name "another-val"
                                 :match "(println another-val some-val)))"))
            :to-equal "(defn my-function []
  (let [some-val 110]
    (println 321 some-val)))
"))

  (it "inlines a function into its calls and leaves a fn where it's passed around"
    (expect (cljr-test-inline "(defn my-inc [n]
  (+ 1 n))

(+ (my-inc (- 17 4)) 55)

(map my-inc (range 10))"
              (cljr-test-in-file :line-beg 1 :line-end 2 :col-beg 1 :col-end 11
                                 :name "refactor-nrepl.foo/my-inc"
                                 :match "(defn my-inc [n]\n  (+ 1 n))"
                                 :definition "(fn [n]\n  (+ 1 n))")
              (cljr-test-in-file :line-beg 4 :line-end 4 :col-beg 5 :col-end 12
                                 :name "refactor-nrepl.foo/my-inc"
                                 :match "(+ (my-inc (- 17 4) 55))")
              (cljr-test-in-file :line-beg 6 :line-end 6 :col-beg 6 :col-end 13
                                 :name "refactor-nrepl.foo/my-inc"
                                 :match "(map my-inc (range 10))"))
            :to-equal "(+ (+ 1 (- 17 4)) 55)

(map (fn [n]
  (+ 1 n)) (range 10))
"))

  (it "inlines a def whose value is a function call"
    (expect (cljr-test-inline "(def trim-lower (comp str/lower-case str/trim))

(trim-lower a-string)"
              (cljr-test-in-file :line-beg 1 :line-end 1 :col-beg 1 :col-end 48
                                 :name "refactor-nrepl.foo/trim-lower"
                                 :match "(def trim-lower (comp str/lower-case str/trim))"
                                 :definition "(comp str/lower-case str/trim)")
              (cljr-test-in-file :line-beg 3 :line-end 3 :col-beg 2 :col-end 26
                                 :name "refactor-nrepl.foo/trim-lower"
                                 :match "(trim-lower a-string)"))
            :to-equal "((comp str/lower-case str/trim) a-string)
"))

  (it "saves the file"
    (cljr-test-with-project ()
      (cljr-test-visit cljr-test-inline-file "(def answer 42)

(inc answer)")
      (cljr--inline-symbol
       (cljr-test-in-file :line-beg 1 :line-end 1 :col-beg 1 :col-end 16
                          :name "example.core/answer"
                          :match "(def answer 42)"
                          :definition "42")
       (list (cljr-test-in-file :line-beg 3 :line-end 3 :col-beg 6 :col-end 12
                                :name "example.core/answer"
                                :match "(inc answer)")))
      (expect (buffer-modified-p) :to-be nil)
      (expect (cljr-test-file-string cljr-test-inline-file 'on-disk)
              :to-equal "(inc 42)\n"))))

(describe "cljr-inline-symbol"
  (before-each
    (cljr-test-without-middleware)
    (spy-on 'cljr--call-middleware-to-find-used-locals :and-return-value "")
    (spy-on 'cider-var-info :and-return-value nil))

  (it "asks the middleware for the definition of the symbol at point and inlines it"
    (cljr-test-with-project ()
      (cljr-test-visit cljr-test-inline-file "(ns example.core)

(def answer 42)

(inc |answer)")
      (cljr-test-middleware-answers (cljr-test-definition-of-answer 'used))
      (let ((cljr--post-command-messages nil))
        (cljr-inline-symbol)
        (expect cljr--post-command-messages
                :to-equal '("Inlined 1 occurrence(s) of 'answer'")))
      (expect (cljr-test-buffer-string t) :to-equal "(ns example.core)

(inc |42)
")
      (expect (cljr-test-middleware-request "name") :to-equal "answer")
      (expect (cljr-test-middleware-request "ns") :to-equal "example.core")
      (expect (cljr-test-middleware-request "line") :to-equal 5)
      (expect (cljr-test-middleware-request "column") :to-equal 6)
      (expect (cljr-test-middleware-request "file")
              :to-equal (cljr-test-file cljr-test-inline-file))))

  (it "only removes the definition when the symbol isn't used"
    (cljr-test-with-project ()
      (cljr-test-visit cljr-test-inline-file "(ns example.core)

(def |answer 42)

(inc 1)")
      (cljr-test-middleware-answers (cljr-test-definition-of-answer))
      (let ((cljr--post-command-messages nil))
        (cljr-inline-symbol)
        (expect cljr--post-command-messages
                :to-equal '("No occurrences of 'answer' found.  Deleted the definition.")))
      (expect (cljr-test-buffer-string) :to-equal "(ns example.core)

(inc 1)
")))

  (it "leaves the file alone when the user turns the preview down"
    (cljr-test-with-project ()
      (cljr-test-visit cljr-test-inline-file "(ns example.core)

(def answer 42)

(inc |answer)")
      (save-buffer)
      (cljr-test-middleware-answers (cljr-test-definition-of-answer 'used))
      (spy-on 'y-or-n-p :and-return-value nil)
      (let ((cljr-preview-refactorings t)
            (cljr--post-command-messages nil))
        (cljr-inline-symbol)
        (expect cljr--post-command-messages :to-be nil))
      (expect 'y-or-n-p :to-have-been-called-times 1)
      (expect (cljr-test-buffer-string) :to-equal "(ns example.core)

(def answer 42)

(inc answer)
"))))

;;; inline-symbol-test.el ends here
