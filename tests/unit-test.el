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
        (cljr-slash))
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
        (cljr-slash))
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
        (cljr-slash))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [baz.example :as ex :refer [a b c] ]))
ex/"))))

(describe "cljr-slash offline fallback"
  (it "inserts a require from the static table when the middleware is unavailable"
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value nil)
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(ns foo)\nstr|"
        (cljr-slash))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [clojure.string :as str]))
str/")))
  (it "falls back (rather than erroring) with no REPL connected"
    ;; Regression: `cljr--op-supported-p' used to signal `No linked CIDER
    ;; sessions' when disconnected, so typing `/' offline threw instead of
    ;; falling back to the static table.
    (spy-on 'cider-connected-p :and-return-value nil)
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(ns foo)\nstr|"
        (cljr-slash))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [clojure.string :as str]))
str/"))))

(describe "cljr--op-supported-p"
  (it "returns nil (rather than erroring) when no REPL is connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cljr--op-supported-p "clean-ns") :to-be nil)))

(describe "cljr-clean-ns offline fallback"
  ;; Drive the real `cljr--op-supported-p' by faking a disconnected REPL,
  ;; instead of stubbing the predicate the fallback hinges on.
  (before-each (spy-on 'cider-connected-p :and-return-value nil))
  (it "sorts the ns form when no REPL is connected"
    (spy-on 'cljr--post-command-message)
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo\n  (:require [b.b :as b]\n            [a.a :as a]))\n")
      (cljr-clean-ns)
      (expect (buffer-string) :to-equal "(ns foo
  (:require [a.a :as a]
            [b.b :as b]))
")))
  (it "doesn't touch the middleware when no REPL is connected"
    (spy-on 'cljr--clean-ns)
    (spy-on 'cider-eval-ns-form)
    (spy-on 'cljr--post-command-message)
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo\n  (:require [a.a :as a]))\n")
      (cljr-clean-ns)
      (expect 'cljr--clean-ns :not :to-have-been-called)
      (expect 'cider-eval-ns-form :not :to-have-been-called))))

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

(describe "cljr--cache-artifacts-from-response"
  (it "populates the Emacs-side artifact cache from an async response"
    (let ((cljr--artifacts-cache nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (cljr--cache-artifacts-from-response (nrepl-dict "artifacts" '("a/a" "b/b"))))
      (expect (cdr cljr--artifacts-cache) :to-equal '("a/a" "b/b"))))
  (it "ignores responses without an artifacts entry"
    (let ((cljr--artifacts-cache nil))
      (cljr--cache-artifacts-from-response (nrepl-dict "status" '("done")))
      (expect cljr--artifacts-cache :to-be nil)))
  (it "lets a warmed cache serve without a middleware round-trip"
    (let ((cljr-artifact-cache-ttl 300)
          (cljr--artifacts-cache nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (cljr--cache-artifacts-from-response (nrepl-dict "artifacts" '("warmed"))))
      (spy-on 'cljr--call-middleware-sync)
      (expect (cljr--get-artifacts-from-middleware nil) :to-equal '("warmed"))
      (expect 'cljr--call-middleware-sync :not :to-have-been-called))))

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

(describe "cljr-add-missing-libspec (offline fallback)"
  (it "requires an aliased symbol via the table when resolve-missing is unavailable"
    (spy-on 'cljr--op-supported-p :and-return-value nil)
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value nil)
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(ns foo)\n(str/joi|n [])"
        (cljr-add-missing-libspec))
      (expect (buffer-string) :to-equal "(ns foo
  (:require [clojure.string :as str]))
(str/join [])")))
  (it "errors on a non-alias symbol when resolve-missing is unavailable"
    (spy-on 'cljr--op-supported-p :and-return-value nil)
    (spy-on 'cljr--slash-suggest-op-available-p :and-return-value nil)
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(ns foo)\n(printl|n :x)"
        (expect (cljr-add-missing-libspec) :to-throw 'user-error)))))

(describe "cljr--already-declared-p"
  (it "matches declared names with regexp specials, not substrings of a symbol"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(ns foo)\n(declare *db* valid?)")
      (expect (cljr--already-declared-p "*db*") :to-be-truthy)
      (expect (cljr--already-declared-p "valid?") :to-be-truthy)
      ;; these are substrings of the declared symbols, not declared themselves
      (expect (cljr--already-declared-p "db") :to-be nil)
      (expect (cljr--already-declared-p "valid") :to-be nil))))
(describe "cljr-cycle-thread"
  (it "cycles -> to ->>"
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(-> x| foo)"
        (cljr-cycle-thread))
      (expect (buffer-string) :to-equal "(->> x foo)")))
  (it "errors when point isn't in a threading macro"
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(println| :x)"
        (expect (cljr-cycle-thread) :to-throw 'user-error)))))

(describe "cljr-promote-function"
  (it "promotes a #() literal to (fn ...) without touching the middleware"
    ;; the literal->fn path is a local edit and must not require a REPL
    (spy-on 'cljr--ensure-op-supported
            :and-call-fake (lambda (&rest _) (error "should not need the middleware")))
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(map |#(foo) xs)"
        (cljr-promote-function nil))
      (expect (buffer-string) :to-equal "(map (fn [] (foo)) xs)"))))
(describe "cljr-remove-let"
  (before-each
    ;; the command is a local edit and must not reach for the middleware
    (spy-on 'cljr--ensure-op-supported
            :and-call-fake (lambda (&rest _) (error "should not need the middleware"))))
  (it "inlines a single binding"
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(let [x 1] (+ x| x))"
        (cljr-remove-let))
      (expect (string-trim (buffer-string)) :to-equal "(+ 1 1)")))
  (it "inlines multiple independent bindings"
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(let [x 1 y 2] (+ x| y))"
        (cljr-remove-let))
      (expect (string-trim (buffer-string)) :to-equal "(+ 1 2)")))
  (it "resolves bindings that depend on earlier ones"
    (cljr--with-clojure-temp-file "foo.clj"
      (with-point-at "(let [x 1 y (+ x 1)] (* y| y))"
        (cljr-remove-let))
      (expect (string-trim (buffer-string)) :to-equal "(* (+ 1 1) (+ 1 1))"))))

;; ---- change-function-signature: tagged (add/remove) model ----

(defun cljr--test-keep (old new name &optional new-name)
  (let ((h (make-hash-table)))
    (puthash :op :keep h)
    (puthash :old-index old h)
    (puthash :new-index new h)
    (puthash :old-name name h)
    (puthash :new-name (or new-name name) h)
    h))

(defun cljr--test-add (new name &optional default)
  (let ((h (make-hash-table)))
    (puthash :op :add h)
    (puthash :new-index new h)
    (puthash :new-name name h)
    (puthash :default (or default "nil") h)
    h))

(defun cljr--test-remove (old name)
  (let ((h (make-hash-table)))
    (puthash :op :remove h)
    (puthash :old-index old h)
    (puthash :old-name name h)
    h))

(describe "cljr--update-function-signature (tagged model)"
  (it "adds a parameter at the end of the definition"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(defn tt [foo bar]\n  (println foo bar))")
      (goto-char (point-min))
      (cljr--update-function-signature
       (list (cljr--test-keep 0 0 "foo")
             (cljr--test-keep 1 1 "bar")
             (cljr--test-add 2 "qux")))
      (expect (buffer-string) :to-equal
              "(defn tt [foo bar qux]\n  (println foo bar))")))
  (it "adds a parameter at the front of the definition"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(defn tt [foo bar]\n  (println foo bar))")
      (goto-char (point-min))
      (cljr--update-function-signature
       (list (cljr--test-add 0 "x")
             (cljr--test-keep 0 1 "foo")
             (cljr--test-keep 1 2 "bar")))
      (expect (buffer-string) :to-equal
              "(defn tt [x foo bar]\n  (println foo bar))")))
  (it "removes a parameter from the definition"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(defn tt [foo bar baz]\n  (println foo baz))")
      (goto-char (point-min))
      (cljr--update-function-signature
       (list (cljr--test-keep 0 0 "foo")
             (cljr--test-remove 1 "bar")
             (cljr--test-keep 2 1 "baz")))
      (expect (buffer-string) :to-equal
              "(defn tt [foo baz]\n  (println foo baz))"))))

(describe "cljr--update-call-site (tagged model)"
  (it "inserts the placeholder for an added trailing parameter"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(tt 1 2)")
      (goto-char (point-min))
      (forward-char 1)
      (cljr--update-call-site
       (list (cljr--test-keep 0 0 "foo")
             (cljr--test-keep 1 1 "bar")
             (cljr--test-add 2 "qux" "nil")))
      (expect (buffer-string) :to-equal "(tt 1 2 nil)")))
  (it "inserts the placeholder for an added leading parameter"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(tt 1 2)")
      (goto-char (point-min))
      (forward-char 1)
      (cljr--update-call-site
       (list (cljr--test-add 0 "x" "nil")
             (cljr--test-keep 0 1 "foo")
             (cljr--test-keep 1 2 "bar")))
      (expect (buffer-string) :to-equal "(tt nil 1 2)"))))

(describe "cljr--signature helpers"
  (it "defaults a tagless entry to :keep"
    (let ((tagless (make-hash-table)))
      (puthash :old-index 0 tagless)
      (expect (cljr--change-op tagless) :to-be :keep)))
  (it "detects adds and removes"
    (let ((cs (list (cljr--test-keep 0 0 "foo")
                    (cljr--test-add 1 "bar")
                    (cljr--test-remove 2 "baz"))))
      (expect (cljr--signature-has-add-p cs) :to-be-truthy)
      (expect (cljr--signature-has-remove-p cs) :to-be-truthy)
      (expect (cljr--old-arity cs) :to-equal 2)))
  (it "treats a pure rename as no structural change"
    (expect (cljr--signature-structurally-changed-p
             (list (cljr--test-keep 0 0 "foo" "renamed")))
            :to-be nil)))

(describe "cljr--parse-arglists"
  (it "parses a single arity"
    (expect (cljr--parse-arglists "[name greeting]")
            :to-equal '(("name" "greeting"))))
  (it "parses a nullary arity"
    (expect (cljr--parse-arglists "[]") :to-equal '(nil)))
  (it "keeps the rest marker as its own element"
    (expect (cljr--parse-arglists "[a & more]")
            :to-equal '(("a" "&" "more"))))
  (it "parses multiple arities separated by newlines"
    (expect (cljr--parse-arglists "[x]\n[x y]")
            :to-equal '(("x") ("x" "y")))))

(describe "cljr--signature-variadic-p"
  (it "detects a rest parameter"
    (expect (cljr--signature-variadic-p
             (list (cljr--test-keep 0 0 "a")
                   (cljr--test-keep 1 1 "&")
                   (cljr--test-keep 2 2 "more")))
            :to-be-truthy))
  (it "is nil for a fixed-arity signature"
    (expect (cljr--signature-variadic-p
             (list (cljr--test-keep 0 0 "a") (cljr--test-keep 1 1 "b")))
            :to-be nil)))

(describe "cljr--count-lambda-list-params"
  (it "counts params in a lambda list"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "[a b c]")
      (goto-char (point-min))
      (expect (cljr--count-lambda-list-params) :to-equal 3)))
  (it "counts the rest marker as a param"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "[a & more]")
      (goto-char (point-min))
      (expect (cljr--count-lambda-list-params) :to-equal 3)))
  (it "skips schema type annotations"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "[a :- s/Str b :- s/Int]")
      (goto-char (point-min))
      (expect (cljr--count-lambda-list-params) :to-equal 2))))

(describe "cljr--call-site-arg-count"
  (it "counts arguments at a call site"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(foo 1 2 3)")
      (goto-char (point-min))
      (forward-char 1)
      (expect (cljr--call-site-arg-count) :to-equal 3)))
  (it "returns 0 for a no-arg call"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(foo)")
      (goto-char (point-min))
      (forward-char 1)
      (expect (cljr--call-site-arg-count) :to-equal 0))))

(describe "cljr--choose-arity"
  (it "returns the sole arity without prompting"
    (expect (cljr--choose-arity '(("a" "b"))) :to-equal '("a" "b")))
  (it "prompts and returns the chosen arity when there are several"
    (spy-on 'completing-read :and-return-value "[x y]")
    (expect (cljr--choose-arity '(("x") ("x" "y"))) :to-equal '("x" "y"))))

(describe "cljr--update-function-signature (multi-arity)"
  (it "reorders only the chosen arity's lambda list"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(defn multi\n  ([x] (multi x 0))\n  ([x y] (+ x y)))")
      (goto-char (point-min))
      (cljr--update-function-signature
       (list (cljr--test-keep 0 1 "x")
             (cljr--test-keep 1 0 "y")))
      (expect (buffer-string) :to-equal
              "(defn multi\n  ([x] (multi x 0))\n  ([y x] (+ x y)))")))
  (it "adds a parameter to the chosen arity only"
    (cljr--with-clojure-temp-file "foo.clj"
      (insert "(defn multi\n  ([x] x)\n  ([x y] (+ x y)))")
      (goto-char (point-min))
      (cljr--update-function-signature
       (list (cljr--test-keep 0 0 "x")
             (cljr--test-keep 1 1 "y")
             (cljr--test-add 2 "z")))
      (expect (buffer-string) :to-equal
              "(defn multi\n  ([x] x)\n  ([x y z] (+ x y)))"))))

(describe "cljr--run-previewable-refactoring"
  (let (tmpfile)
    (defun cljr--test-file-contents (f)
      (with-temp-buffer (insert-file-contents f) (buffer-string)))
    (defun cljr--test-rename-thunk (f)
      "A previewable edit: replace foo with bar in F."
      (lambda ()
        (with-current-buffer (cljr--find-file-noselect f)
          (goto-char (point-min))
          (while (search-forward "foo" nil t) (replace-match "bar"))
          (cljr--save-buffer))))
    (before-each
      (setq tmpfile (make-temp-file "cljr-preview" nil ".clj" "(def foo 1)\n(foo)\n"))
      (setq cljr--last-refactoring nil))
    (after-each
      (when (get-file-buffer tmpfile)
        (with-current-buffer (get-file-buffer tmpfile)
          (set-buffer-modified-p nil)
          (kill-buffer)))
      (ignore-errors (delete-file tmpfile)))

    (it "applies the edits to disk when confirmed"
      (spy-on 'y-or-n-p :and-return-value t)
      (let ((cljr-preview-refactorings t))
        (expect (cljr--run-previewable-refactoring "test" (cljr--test-rename-thunk tmpfile))
                :to-be-truthy))
      (expect (cljr--test-file-contents tmpfile) :to-equal "(def bar 1)\n(bar)\n"))

    (it "leaves the file untouched when declined"
      (spy-on 'y-or-n-p :and-return-value nil)
      (let ((cljr-preview-refactorings t))
        (expect (cljr--run-previewable-refactoring "test" (cljr--test-rename-thunk tmpfile))
                :to-be nil))
      (expect (cljr--test-file-contents tmpfile) :to-equal "(def foo 1)\n(foo)\n"))

    (it "applies directly (no prompt) when preview is disabled"
      (spy-on 'y-or-n-p)
      (let ((cljr-preview-refactorings nil))
        (cljr--run-previewable-refactoring "test" (cljr--test-rename-thunk tmpfile)))
      (expect 'y-or-n-p :not :to-have-been-called)
      (expect (cljr--test-file-contents tmpfile) :to-equal "(def bar 1)\n(bar)\n"))

    (it "records an applied refactoring so it can be undone"
      (spy-on 'y-or-n-p :and-return-value t)
      (spy-on 'yes-or-no-p :and-return-value t)
      (let ((cljr-preview-refactorings t))
        (cljr--run-previewable-refactoring "test" (cljr--test-rename-thunk tmpfile)))
      (expect (cljr--test-file-contents tmpfile) :to-equal "(def bar 1)\n(bar)\n")
      (cljr-undo-last-refactoring)
      (expect (cljr--test-file-contents tmpfile) :to-equal "(def foo 1)\n(foo)\n"))

    (it "preserves a target buffer's unrelated unsaved edits on abort"
      ;; The buffer is dirty for reasons unrelated to the refactoring; declining
      ;; must restore that content AND leave it marked modified (not silently
      ;; mark it saved, which would lose the edits) and must not touch disk.
      (spy-on 'y-or-n-p :and-return-value nil)
      (let ((cljr-preview-refactorings t)
            (buf (find-file-noselect tmpfile)))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert ";; unsaved work\n")
          (expect (buffer-modified-p) :to-be-truthy))
        (cljr--run-previewable-refactoring "test" (cljr--test-rename-thunk tmpfile))
        (with-current-buffer buf
          (expect (buffer-string) :to-equal "(def foo 1)\n(foo)\n;; unsaved work\n")
          (expect (buffer-modified-p) :to-be-truthy))
        (expect (cljr--test-file-contents tmpfile) :to-equal "(def foo 1)\n(foo)\n")))))

(describe "cljr--call-middleware-async-collect"
  (it "accumulates split response messages and delivers the value on done"
    (spy-on 'cljr--call-middleware-async :and-call-fake
            (lambda (_request cb)
              (funcall cb (nrepl-dict "touched" '("a.clj" "b.clj")))
              (funcall cb (nrepl-dict "status" '("done")))))
    (let (result (calls 0))
      (cljr--call-middleware-async-collect
       (cljr--create-msg "rename-file-or-dir") "touched"
       (lambda (v) (setq result v calls (1+ calls))))
      (expect calls :to-equal 1)
      (expect result :to-equal '("a.clj" "b.clj"))))
  (it "fires once when value and done arrive together"
    (spy-on 'cljr--call-middleware-async :and-call-fake
            (lambda (_request cb)
              (funcall cb (nrepl-dict "touched" '("a.clj") "status" '("done")))))
    (let ((calls 0))
      (cljr--call-middleware-async-collect
       (cljr--create-msg "x") "touched" (lambda (_v) (setq calls (1+ calls))))
      (expect calls :to-equal 1)))
  (it "doesn't fire before the request is done"
    (spy-on 'cljr--call-middleware-async :and-call-fake
            (lambda (_request cb)
              (funcall cb (nrepl-dict "touched" '("a.clj")))))
    (let ((calls 0))
      (cljr--call-middleware-async-collect
       (cljr--create-msg "x") "touched" (lambda (_v) (setq calls (1+ calls))))
      (expect calls :to-equal 0))))

(describe "cljr--find-symbol-occurrences"
  (before-each
    ;; cljr--find-symbol pokes cider-current-repl and cljr--get-valid-filename
    ;; touches tramp/nrepl filename fns; stub those out.
    (spy-on 'cider-current-repl :and-return-value (current-buffer))
    (spy-on 'cljr--get-valid-filename :and-call-fake
            (lambda (occ) (gethash :file occ))))
  (it "collects occurrences and delivers them on the count message"
    (spy-on 'cljr--find-symbol :and-call-fake
            (lambda (_symbol _ns cb)
              (funcall cb (nrepl-dict "occurrence" "{:file \"a.clj\" :line-beg 1 :col-beg 1 :name \"x\"}"))
              (funcall cb (nrepl-dict "occurrence" "{:file \"a.clj\" :line-beg 2 :col-beg 3 :name \"x\"}"))
              (funcall cb (nrepl-dict "count" 2))))
    (let (result done)
      (cljr--find-symbol-occurrences "x" "ns" (lambda (occs) (setq result occs done t)))
      (with-timeout (2) (while (not done) (sit-for 0.02)))
      (expect (length result) :to-equal 2)
      (expect (gethash :line-beg (car result)) :to-equal 1)))
  (it "drops duplicate occurrences at the same location"
    (spy-on 'cljr--find-symbol :and-call-fake
            (lambda (_symbol _ns cb)
              (funcall cb (nrepl-dict "occurrence" "{:file \"a.clj\" :line-beg 1 :col-beg 1 :name \"x\"}"))
              (funcall cb (nrepl-dict "occurrence" "{:file \"a.clj\" :line-beg 1 :col-beg 1 :name \"x\"}"))
              (funcall cb (nrepl-dict "count" 2))))
    (let (result done)
      (cljr--find-symbol-occurrences "x" "ns" (lambda (occs) (setq result occs done t)))
      (with-timeout (2) (while (not done) (sit-for 0.02)))
      (expect (length result) :to-equal 1)))
  (it "reports a middleware error and doesn't run the callback"
    (spy-on 'cljr--find-symbol :and-call-fake
            (lambda (_symbol _ns cb)
              (funcall cb (nrepl-dict "err" "boom"))))
    (spy-on 'message)
    (let ((called nil))
      (cljr--find-symbol-occurrences "x" "ns" (lambda (_occs) (setq called t)))
      (sit-for 0.05)
      (expect called :to-be nil)
      (expect 'message :to-have-been-called)))
  (it "delivers on the done status when no count is sent"
    (spy-on 'cljr--find-symbol :and-call-fake
            (lambda (_symbol _ns cb)
              (funcall cb (nrepl-dict "occurrence" "{:file \"a.clj\" :line-beg 1 :col-beg 1 :name \"x\"}"))
              (funcall cb (nrepl-dict "status" '("done")))))
    (let (result done)
      (cljr--find-symbol-occurrences "x" "ns" (lambda (occs) (setq result occs done t)))
      (with-timeout (2) (while (not done) (sit-for 0.02)))
      (expect (length result) :to-equal 1)))
  (it "starts a spinner and stops it when the search completes"
    (spy-on 'cider-spinner-start)
    (spy-on 'cljr--stop-spinner)
    (spy-on 'cljr--find-symbol :and-call-fake
            (lambda (_symbol _ns cb) (funcall cb (nrepl-dict "count" 0))))
    (cljr--find-symbol-occurrences "x" "ns" #'ignore)
    (expect 'cider-spinner-start :to-have-been-called)
    (expect 'cljr--stop-spinner :to-have-been-called)))
