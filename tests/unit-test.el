;;; unit-test.el --- Unit tests for clj-refactor -*- lexical-binding: t; -*-

(require 'paredit)
(require 'clj-refactor)
(require 'buttercup)
(require 'cl-lib)

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
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value t)
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
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value t)
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
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value t)
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

(describe "cljr-slash offline fallback"
  (it "inserts a require from the static table when the middleware is unavailable"
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value nil)
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(ns foo)\nstr|"
        (let ((cljr-slash-uses-suggest-libspec t))
          (cljr-slash)))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [clojure.string :as str]))
str/"))))

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

(describe "clj-refactor-menu"
  (it "is a transient prefix"
    (expect (get 'clj-refactor-menu 'transient--prefix) :not :to-be nil))
  (it "exposes every command from `cljr--all-helpers' under its own key"
    ;; Guards against adding a command to `cljr--all-helpers' but forgetting
    ;; to list it in the transient menu.
    (dolist (entry cljr--all-helpers)
      (let ((key (car entry))
            (command (cadr entry)))
        ;; The menu itself is bound in `cljr--all-helpers' but is the entry
        ;; point, so it isn't one of the menu's own suffixes.
        (unless (eq command 'clj-refactor-menu)
          (let ((suffix (ignore-errors (transient-get-suffix 'clj-refactor-menu key))))
            (expect suffix :not :to-be nil)))))))

(describe "cljr--with-opened-buffers"
  (it "kills buffers it opens but leaves already-visited ones alone"
    (let* ((already (make-temp-file "cljrtmp" nil ".txt" "already\n"))
           (fresh (make-temp-file "cljrtmp" nil ".txt" "fresh\n"))
           (already-buf (find-file-noselect already)))
      (unwind-protect
          (progn
            (cljr--with-opened-buffers
              (cljr--find-file-noselect already)
              (cljr--find-file-noselect fresh))
            (expect (buffer-live-p already-buf) :to-be-truthy)
            (expect (get-file-buffer fresh) :to-be nil))
        (when (buffer-live-p already-buf) (kill-buffer already-buf))
        (when (get-file-buffer fresh) (kill-buffer (get-file-buffer fresh)))
        (delete-file already)
        (delete-file fresh)))))

(describe "cljr--cache-fresh-p"
  (it "honors the TTL and the disable setting"
    (let ((cljr-artifact-cache-ttl 300))
      (expect (cljr--cache-fresh-p (cons (float-time) '("x"))) :to-be-truthy)
      (expect (cljr--cache-fresh-p (cons (- (float-time) 10000) '("x"))) :to-be nil)
      (expect (cljr--cache-fresh-p nil) :to-be nil))
    (let ((cljr-artifact-cache-ttl nil))
      (expect (cljr--cache-fresh-p (cons (float-time) '("x"))) :to-be nil))))

(describe "cljr--get-artifacts-from-middleware"
  (it "caches within the TTL and refetches when forced"
    (let ((cljr-artifact-cache-ttl 300)
          (cljr--artifacts-cache nil)
          (calls 0))
      (cl-letf (((symbol-function 'cljr--call-middleware-sync)
                 (lambda (&rest _) (setq calls (1+ calls)) '("a" "b")))
                ((symbol-function 'message) #'ignore))
        (expect (cljr--get-artifacts-from-middleware nil) :to-equal '("a" "b"))
        (expect (cljr--get-artifacts-from-middleware nil) :to-equal '("a" "b"))
        (expect calls :to-equal 1)
        (cljr--get-artifacts-from-middleware t)
        (expect calls :to-equal 2)))))

(describe "cljr--call-middleware-suggest-libspec"
  (it "caches suggestions per alias within the TTL"
    (let ((cljr-artifact-cache-ttl 300)
          (cljr--suggest-libspecs-cache (make-hash-table :test #'equal))
          (calls 0))
      (cl-letf (((symbol-function 'cljr--ensure-op-supported) (lambda (op) op))
                ((symbol-function 'cljr--call-middleware-sync)
                 (lambda (&rest _) (setq calls (1+ calls)) "([clojure.set :as set])")))
        (let ((first (cljr--call-middleware-suggest-libspec "set" '("clj" nil)))
              (second (cljr--call-middleware-suggest-libspec "set" '("clj" nil))))
          (expect calls :to-equal 1)
          (expect second :to-equal first))
        ;; a different alias is a cache miss
        (cljr--call-middleware-suggest-libspec "io" '("clj" nil))
        (expect calls :to-equal 2)))))

(describe "cljr--magic-require-libspec-from-table"
  (it "resolves a known alias in a clj file"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)")
      (expect (cljr--magic-require-libspec-from-table "str")
              :to-equal "[clojure.string :as str]")
      (expect (cljr--magic-require-libspec-from-table "io")
              :to-equal "[clojure.java.io :as io]")))
  (it "honors an entry's :only language context"
    (cljr--with-clojure-temp-file "foo.cljs"
      (insert "(ns foo)")
      ;; io is restricted to clj, so it does not apply in a cljs file
      (expect (cljr--magic-require-libspec-from-table "io") :to-be nil)
      ;; but an unrestricted alias still resolves
      (expect (cljr--magic-require-libspec-from-table "str")
              :to-equal "[clojure.string :as str]")))
  (it "is permissive in cljc files"
    (cljr--with-clojure-temp-file "foo.cljc"
      (insert "(ns foo)")
      (expect (cljr--magic-require-libspec-from-table "io")
              :to-equal "[clojure.java.io :as io]")))
  (it "returns nil for an unknown alias"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)")
      (expect (cljr--magic-require-libspec-from-table "nope") :to-be nil))))

(describe "cljr--magic-require-artifact"
  (it "reads the :artifact coordinate from a table entry"
    (let ((cljr-magic-require-namespaces
           '(("json" "cheshire.core" :artifact "cheshire/cheshire")
             ("str" . "clojure.string"))))
      (expect (cljr--magic-require-artifact "json") :to-equal "cheshire/cheshire")
      (expect (cljr--magic-require-artifact "str") :to-be nil)
      (expect (cljr--magic-require-artifact "nope") :to-be nil))))

(describe "cljr--libspec-ns"
  (it "extracts the namespace from a libspec string"
    (expect (cljr--libspec-ns "[cheshire.core :as json]") :to-equal "cheshire.core")
    (expect (cljr--libspec-ns "[foo.bar :as fb :refer [a b]]") :to-equal "foo.bar")))

(describe "cljr--slash-maybe-add-missing-lib"
  (before-each
    (spy-on 'cljr--add-project-dependency)
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cljr--get-versions-from-middleware :and-return-value '("1.2.3"))
    (spy-on 'y-or-n-p :and-return-value t))
  (it "adds the configured artifact when the namespace is missing"
    (spy-on 'cljr--ns-on-classpath-p :and-return-value nil)
    (let ((cljr-slash-add-missing-libs t)
          (cljr-magic-require-namespaces
           '(("json" "cheshire.core" :artifact "cheshire/cheshire"))))
      (cljr--slash-maybe-add-missing-lib "json" "[cheshire.core :as json]")
      (expect 'cljr--add-project-dependency
              :to-have-been-called-with "cheshire/cheshire" "1.2.3")))
  (it "does nothing when the namespace is already on the classpath"
    (spy-on 'cljr--ns-on-classpath-p :and-return-value "/path/cheshire/core.clj")
    (let ((cljr-slash-add-missing-libs t)
          (cljr-magic-require-namespaces
           '(("json" "cheshire.core" :artifact "cheshire/cheshire"))))
      (cljr--slash-maybe-add-missing-lib "json" "[cheshire.core :as json]")
      (expect 'cljr--add-project-dependency :not :to-have-been-called)))
  (it "does nothing when disabled"
    (spy-on 'cljr--ns-on-classpath-p :and-return-value nil)
    (let ((cljr-slash-add-missing-libs nil)
          (cljr-magic-require-namespaces
           '(("json" "cheshire.core" :artifact "cheshire/cheshire"))))
      (cljr--slash-maybe-add-missing-lib "json" "[cheshire.core :as json]")
      (expect 'cljr--add-project-dependency :not :to-have-been-called)))
  (it "does nothing when no :artifact is configured"
    (spy-on 'cljr--ns-on-classpath-p :and-return-value nil)
    (let ((cljr-slash-add-missing-libs t)
          (cljr-magic-require-namespaces '(("str" . "clojure.string"))))
      (cljr--slash-maybe-add-missing-lib "str" "[clojure.string :as str]")
      (expect 'cljr--add-project-dependency :not :to-have-been-called))))

(describe "cljr-require-alias-at-point"
  (it "requires the unresolved alias of the symbol at point (offline)"
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value nil)
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(ns foo)\n(str/joi|n [])"
        (cljr-require-alias-at-point))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [clojure.string :as str]))
(str/join [])")))
  (it "errors when there is no unresolved alias at point"
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value nil)
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(ns foo)\n(printl|n :x)"
        (expect (cljr-require-alias-at-point) :to-throw 'user-error)))))
