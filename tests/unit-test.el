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

(defmacro cljr--with-clojure-temp-file (filename &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (setq-local buffer-file-name ,filename)
     (delay-mode-hooks (clojure-mode))
     ,@body))

(defmacro with-point-at (text &rest body)
  (declare (indent 1))
  `(progn (insert ,text)
          (goto-char (point-min))
          (re-search-forward "|")
          (delete-char -1)
          ,@body))

(describe "cljr--insert-require-libspec"
  :var (after-inserting-a
        after-inserting-b)
  (before-each
    (cljr--with-clojure-temp-file "libspec.clj"
      (insert "(ns lib)\n")
      (cljr--insert-require-libspec "[com.a :as a]")
      (setq after-inserting-a (buffer-string))
      (cljr--insert-require-libspec "[com.b :as b]")
      (setq after-inserting-b (buffer-string))))

  (it "adds a new require over an empty ns form"
    (expect after-inserting-a :to-equal
            "(ns lib\n  (:require [com.a :as a]))\n"))
  (it "adds a new require over a ns with existing requires"
    (expect after-inserting-b :to-equal
            "(ns lib
  (:require [com.a :as a]
            [com.b :as b]))
")))

(defun cljr--alias-here (content)
  (with-temp-buffer
    (clojure-mode) ;; activate clojure syntax table
    (insert content)
    (cljr--ns-alias-at-point)))

;; https://clojure.org/reference/reader#_reader_forms
(describe "cljr--ns-alias-at-point"
  (it "returns the short alias before the /"
    (expect (cljr--alias-here "aba/")
            :to-equal "aba"))

  (it "does not include boundaries"
    (expect (cljr--alias-here " alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "(alias/")
            :to-equal "alias")
    (expect (cljr--alias-here ",alias/")
            :to-equal "alias"))

  (it "identifies dotted namespace aliases"
    (expect (cljr--alias-here "a.b.c/")
            :to-equal "a.b.c"))

  (it "removes prefix :: (namespaced keyword)"
    (expect (cljr--alias-here "::ns-name/")
            :to-equal "ns-name")
    (expect (cljr--alias-here "(::util/")
            :to-equal "util"))

  (it "includes question-mark"
    (expect (cljr--alias-here "ns-name?/")
            :to-equal "ns-name?")
    (expect (cljr--alias-here "ns?name/")
            :to-equal "ns?name")
    (expect (cljr--alias-here "?ns-name/")
            :to-equal "?ns-name"))

  (it "removes prefix : (keyword)"
    (expect (cljr--alias-here ":ns-name/")
            :to-equal "ns-name")
    (expect (cljr--alias-here "(:alias/")
            :to-equal "alias"))

  (it "allows infix :"
    (expect (cljr--alias-here "foo:bar/")
            :to-equal "foo:bar"))

  (it "allows infix $"
    (expect (cljr--alias-here "foo$bar/")
            :to-equal "foo$bar"))

  (it "removes deref operator"
    (expect (cljr--alias-here "@atom/")
            :to-equal "atom"))

  (it "removes prefix quote"
    (expect (cljr--alias-here "'name-bar.set/")
            :to-equal "name-bar.set"))

  (it "allows infix quote"
    (expect (cljr--alias-here "tl'an/")
            :to-equal "tl'an"))

  (it "removes sharpquote"
    (expect (cljr--alias-here "#'alias/")
            :to-equal "alias"))

  (it "removes quasiquote"
    (expect (cljr--alias-here "`alias/")
            :to-equal "alias"))

  (it "ignores prefix digits"
    (expect (cljr--alias-here "0alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "01alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "{01alias/")
            :to-equal "alias"))

  (it "allows internal & suffix digits"
    (expect (cljr--alias-here "alias0/")
            :to-equal "alias0")
    (expect (cljr--alias-here "ali0as/")
            :to-equal "ali0as"))

  (it "ignores multiple prefixes"
    (expect (cljr--alias-here "'#alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "'#0alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "':alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "#':alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "(#':alias/")
            :to-equal "alias")
    (expect (cljr--alias-here "([#'::alias/")
            :to-equal "alias")))

(describe "cljr--unresolved-alias-ref"
  (it "returns unresolved alias reference"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "ns-name")
              :to-equal "ns-name")))

  (it "returns nil for resolved alias"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo (:require [user.ns-name :as ns-name]))")
      (expect (cljr--unresolved-alias-ref "ns-name")
              :to-be nil)))

  (it "returns nil for js alias in cljs file."
    (cljr--with-clojure-temp-file "foo.cljs"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "js")
              :to-be nil)))

  (it "returns js for js alias in clj file."
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "js")
              :to-equal "js")))

  (it "returns js for js alias in cljc file."
    (cljr--with-clojure-temp-file "foo.cljc"
      (insert "(ns foo)")
      (expect (cljr--unresolved-alias-ref "js")
              :to-equal "js"))))

(describe "cljr--language-context-at-point"
  (it "identifies a clj file"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)")
      (expect (cljr--language-context-at-point)
              :to-equal '("clj" nil))))

  (it "identifies a cljs file"
    (cljr--with-clojure-temp-file "foo.cljs"
      (insert "(ns foo)")
      (expect (cljr--language-context-at-point)
              :to-equal '("cljs" nil))))

  (it "identifies a cljc file"
    (cljr--with-clojure-temp-file "foo.cljc"
      (insert "(ns foo)")
      (expect (cljr--language-context-at-point)
              :to-equal '("cljc" nil))))

  (it "identifies a cljc file with a cljs context"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:cljs (Math/log|))"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "identifies a cljc file with a clj context"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:clj (Math/log|))"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "clj")))))

  (it "identifies a nested context with two branches present"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:cljs (Math/log|) :clj 1)"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "identifies a nested context with an alternate branch preceding"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:clj 1 :cljs (Math/log|))"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "identifies a nested context when point is on symbol"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:clj 1 |:cljs 2)"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "identifies a nested context if point is between symbol and form"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:cljs |1)"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "returns nil in an incomplete reader conditional"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(|)"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" nil)))))

  (it "returns last context in an incomplete reader conditional"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:cljs |)"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "returns preceding context if between two contexts"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?@(:cljs 1| :clj [2])"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "returns :default context if specified"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:default [|])"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "default")))))

  (it "returns :cljr context if specified"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:cljr (|))"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljr")))))

  (it "returns :bb context if specified"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:bb |)"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "bb")))))

  (it "returns :cljs context if specified in #?@ conditional"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?@(:cljs [|])"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "returns closest context if in nested conditional"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo) #?(:default #?(:clj [1|]) :cljs [])"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "clj")))))

  (it "returns context even if sexp is not closed"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "#?(:cljs |"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  (it "returns context even if sexp is succeeded by unbalanced group"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "#?(:cljs |) ("
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" "cljs")))))

  ;; should be '("cljc" "clj") if cljr--beginning-of-reader-conditional handled
  ;; unbalanced preceding parentheses
  (it "can't find context if sexp is preceded by unbalanced group"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at ") #?(:clj |)"
        (expect (cljr--language-context-at-point)
                :to-equal '("cljc" nil))))))

(describe "cljr--beginning-of-reader-conditional"
  (it "returns position of start of reader-conditional if sexp is valid"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "#?(:clj |)"
        (expect (cljr--beginning-of-reader-conditional) :to-equal 4))))

  (it "returns position of start of reader-conditional splice if sexp is valid"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "#?@(:clj |)"
        (expect (cljr--beginning-of-reader-conditional) :to-equal 5))))

  (it "cljr--top-level-p is correct if in a balanced group"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "#?(:clj |)"
        (expect (cljr--top-level-p) :to-be nil))))

  ;; Following examples can't find context as `cljr--top-level-p' erroneously
  ;; reports it is a top level if preceded by unbalanced group
  (it "cljr--top-level-p erroneously passes if preceded by unbalanced group"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at ") #?(:clj |)"
        (expect (cljr--top-level-p) :to-be-truthy))))

  ;; should be 6 if could detect unbalanced preceding parentheses
  (it "can't find position of start if sexp is preceded by unbalanced group"
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at ") #?(:clj |)"
        (expect (cljr--beginning-of-reader-conditional)
                :to-equal nil)))))

(describe "cljr--prompt-or-select-libspec"
  (it "prompts user for namespace selection"
    (spy-on 'completing-read :and-return-value "[a.a :as a]")
    (expect (cljr--prompt-or-select-libspec '("[a.a :as a]"
                                              "[a.b :as b]"))
            :to-equal "[a.a :as a]")
    (expect 'completing-read :to-have-been-called-times 1))

  (it "returns user content from prompt regardless of candidates"
    (spy-on 'completing-read :and-return-value "[alpha :as a]")
    (expect (cljr--prompt-or-select-libspec
             '("[a.a :as a]"
               "[a.b :as b]"))
            :to-equal "[alpha :as a]")
    (expect 'completing-read :to-have-been-called-times 1))

  (it "short-circuits if only one candidate matches"
    (expect (cljr--prompt-or-select-libspec '("[a.a :as a]"))
            :to-equal "[a.a :as a]")
    (expect 'completing-read :to-have-been-called-times 0))

  (it "prompts anyway if `cljr-magic-requires' is `:prompt'"
    (spy-on 'completing-read :and-return-value "[a.a :as a]")
    (let ((cljr-magic-requires :prompt))
      (expect (cljr--prompt-or-select-libspec '("[a.a :as a]"))
              :to-equal "[a.a :as a]"))
    (expect 'completing-read :to-have-been-called-times 1)))

(describe "cljr-slash"
  (it "inserts single selection from suggest-libspec"
    (spy-on 'cljr--call-middleware-suggest-libspec
            :and-return-value (parseedn-read-str "[\"[bar.alias :as alias]\"]"))
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo)\nalias|"
        (let ((cljr-slash-uses-suggest-libspec t))
          (cljr-slash)))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [bar.alias :as alias]))
alias/")))

  (it "prompts user for selection from suggest-libspec"
    (spy-on 'cljr--call-middleware-suggest-libspec
            :and-return-value (parseedn-read-str "[\"[bar.example :as ex]\" \"[baz.example :as ex]\"]"))
    (spy-on 'completing-read
            :and-return-value "[baz.example :as ex]")
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo)\nex|"
        (let ((cljr-slash-uses-suggest-libspec t))
          (cljr-slash)))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [baz.example :as ex]))
ex/")))

  (it "inserts modified user input from completing-read"
    (spy-on 'cljr--call-middleware-suggest-libspec
            :and-return-value (parseedn-read-str "[\"[bar.example :as ex]\" \"[baz.example :as ex]\"]"))
    (spy-on 'completing-read
            :and-return-value "[baz.example :as ex :refer [a b c] ]")
    (cljr--with-clojure-temp-file "foo.cljc"
      (with-point-at "(ns foo)\nex|"
        (let ((cljr-slash-uses-suggest-libspec t))
          (cljr-slash)))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [baz.example :as ex :refer [a b c] ]))
ex/"))))

(describe "cljr--remove-tramp-prefix-from-msg"
  (it "Removes the tramp prefix from specifc nrepl message attributes"
    (with-temp-buffer
      (setq buffer-file-name "/ssh:cider-devs@192.168.50.9#22:a.clj")
      (let* ((v (seq-mapcat #'cljr--remove-tramp-prefix-from-msg
                            (seq-partition (list "file" "/ssh:cider-devs@192.168.50.9#22:a.clj"
                                                 "something" "else")
                                           2))))
        (setq-local tramp-mode nil) ;; prevent hanging
        (expect v :to-equal '("file" "a.clj" "something" "else"))))))
