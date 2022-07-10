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
    (expect (cljr--with-clojure-temp-file "foo.clj"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "ns-name"))
            :to-equal "ns-name"))

  (it "returns nil for resolved alias"
    (expect (cljr--with-clojure-temp-file "foo.clj"
              (insert "(ns foo (:require [user.ns-name :as ns-name]))")
              (cljr--unresolved-alias-ref "ns-name"))
            :to-be nil))

  (it "returns nil for js alias in cljs file."
    (expect (cljr--with-clojure-temp-file "foo.cljs"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "js"))
            :to-be nil))

  (it "returns js for js alias in clj file."
    (expect (cljr--with-clojure-temp-file "foo.clj"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "js"))
            :to-equal "js"))

  (it "returns js for js alias in cljc file."
    (expect (cljr--with-clojure-temp-file "foo.cljc"
              (insert "(ns foo)")
              (cljr--unresolved-alias-ref "js"))
            :to-equal "js")))

(describe "cljr--list-namespace-aliases"
  (it "reduces to a unique list from middleware"
    (spy-on 'cljr--call-middleware-for-namespace-aliases
             :and-return-value
             (parseedn-read-str
              "{:clj  {t (clojure.test) set (clojure.set) sut (alpha shared)}
               :cljs {t (cljs.test) set (clojure.set) sut (beta shared)}}"))
    (expect (cljr--list-namespace-aliases)
            :to-equal
            '((t clojure.test (:clj))
              (sut alpha (:clj))
              (t cljs.test (:cljs))
              (set clojure.set (:clj :cljs))
              (sut beta (:cljs))
              (sut shared (:clj :cljs))))))

(describe "cljr--magic-require-candidates"
  (it "returns an empty list if no matching aliases"
    (spy-on 'cljr--list-namespace-aliases
            :and-return-value '((set clojure.set '(:clj :cljs))))
    (let ((cljr-magic-require-namespaces '(("set" . "clojure.set"))))
      (expect (cljr--magic-require-candidates "alias") :to-equal '())))

  (it "generates candidates from middleware that match"
    (spy-on 'cljr--list-namespace-aliases :and-return-value
            '((gl geometry.line '(:clj :cljs))
              (gl webgl.core '(:cljs))
              (set clojure.set (:clj :cljs))
              (t clojure.test (:clj))
              (t cljs.test (:cljs))))
    ;; single alias to namespace mapping
    (expect (cljr--magic-require-candidates "set")
            :to-equal '((set clojure.set (:clj :cljs))))
    ;; same alias name is re-used across namespaces with different behavior
    (expect (cljr--magic-require-candidates "gl")
            :to-equal '((gl geometry.line '(:clj :cljs))
                        (gl webgl.core '(:cljs))))
    ;; same alias is re-used and a *possible* candidate for reader conditional
    (expect (cljr--magic-require-candidates "t")
            :to-equal '((t clojure.test (:clj))
                        (t cljs.test (:cljs)))))

  (it "fallsback to `cljr-magic-require-namspaces' if middleware does not match."
    (spy-on 'cljr--list-namespace-aliases
            :and-return-value '((set clojure.set '(:clj))))
    (let ((cljr-magic-require-namespaces '(("io" . "clojure.java.io"))))
      (expect (cljr--magic-require-candidates "io")
              :to-equal '((io clojure.java.io ())))))

  (it "returns an empty list if neither match"
    (spy-on 'cljr--list-namespace-aliases :and-return-value '())
    (let ((cljr-magic-require-namespaces '()))
      (expect (cljr--magic-require-candidates "io") :to-equal '()))))

(describe "cljr--alias-completion-table"
  (it "generates prompt table from candidates"
    (let ((table (cljr--alias-completion-table '((a b.a (:clj))
                                                 (b b.b ()))))
          (expected (let ((table (make-hash-table :test 'equal)))
                      (puthash "[b.a :as a] (:clj)" '(a b.a (:clj)) table)
                      (puthash "[b.b :as b]" '(b b.b ()) table)
                      table)))
      (expect (hash-table-keys table) :to-have-same-items-as (hash-table-keys expected))
      (expect (hash-table-values table) :to-have-same-items-as (hash-table-values expected)))))

(describe "cljr--magic-prompt-or-select-namespace"
  (it "prompts user for namespace selection"
    (spy-on 'completing-read :and-return-value "[b.a :as a] (:clj)")
    (expect (cljr--magic-prompt-or-select-namespace '((a b.a (:clj))
                                                      (b b.b (:clj))))
            :to-equal '(a b.a (:clj))))

  (it "returns nil if no selection"
    (spy-on 'completing-read :and-return-value "")
    (expect (cljr--magic-prompt-or-select-namespace
             '((a b.a (:clj))
               (b b.b (:clj))))
            :to-be nil))

  (it "short-circuits if only one candidate matches"
    (expect 'completing-read :to-have-been-called-times 0)
    (expect (cljr--magic-prompt-or-select-namespace '((a b.a (:clj))))
            :to-equal '(a b.a (:clj))))

  (it "prompts anyway if `cljr-magic-requires' is `:prompt'"
    (spy-on 'completing-read :and-return-value "[b.a :as a] (:clj)")
    (let ((cljr-magic-requires :prompt))
      (expect (cljr--magic-prompt-or-select-namespace '((a b.a (:clj))))
              :to-equal '(a b.a (:clj))))))

(describe "cljr-slash"
  (describe "with prompts including context"
    (before-each (setq cljr-magic-require-prompts-includes-context t))
    (after-each (setq cljr-magic-require-prompts-includes-context nil))
    (it "prompts user for require and adds libspec"
      (spy-on 'cljr--call-middleware-for-namespace-aliases
              :and-return-value
              (parseedn-read-str "{:clj {t (clojure.test)} :cljs {t (cljs.test)}}"))
      (spy-on 'completing-read :and-return-value "[cljs.test :as t] (:cljs)")
      (let ((clj-buffer
             (cljr--with-clojure-temp-file "foo.cljc"
               (insert "(ns foo)\n")
               (insert "t")
               (cljr-slash)
               (buffer-string))))
        (expect clj-buffer
                :to-equal "(ns foo\n  (:require [cljs.test :as t]))\nt/")))

    (it "inserts matching libspec without prompting if only a single matching candidate"
      (spy-on 'cljr--call-middleware-for-namespace-aliases
              :and-return-value
              (parseedn-read-str "{:clj {set (clojure.set)} :cljs {set (clojure.set)}}"))
      (expect 'completing-read :to-have-been-called-times 0)
      (let ((clj-buffer
             (cljr--with-clojure-temp-file "foo.cljc"
               (insert "(ns foo)\n")
               (insert "set")
               (cljr-slash)
               (buffer-string))))
        (expect clj-buffer
                :to-equal "(ns foo\n  (:require [clojure.set :as set]))\nset/")))

    (it "only adds / if unknown alias"
      (spy-on 'cljr--call-middleware-for-namespace-aliases
              :and-return-value
              (parseedn-read-str "{}"))
      (expect 'completing-read :to-have-been-called-times 0)
      (let ((clj-buffer
             (cljr--with-clojure-temp-file "foo.cljc"
               (insert "(ns foo)\n")
               (insert "bar")
               (cljr-slash)
               (buffer-string))))
        (expect clj-buffer
                :to-equal "(ns foo)\nbar/")))))
