;;; auto-ns-test.el --- Specs for the ns form of new files -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defmacro cljr-test-with-source-file (&rest body)
  "Evaluate BODY in a project that has an empty src/cljr/core.clj.
That's the file the test files of the specs are taken to test."
  (declare (indent 0) (debug t))
  `(cljr-test-with-project (:name "cljr"
                            :files '(("src/cljr/core.clj" . "")))
     ,@body))

(defun cljr-test-depend-on (dependency)
  "Make DEPENDENCY a development dependency of the test project."
  (cljr-test-write-file
   "project.clj"
   (format "(defproject cljr \"0.1.0-SNAPSHOT\"
  :profiles {:dev {:dependencies [[%s \"1.4\"]]}})" dependency)))

(describe "visiting a blank Clojure file"
  (it "inserts the ns form of a source file"
    (cljr-test-with-source-file
      (cljr-test-visit "src/cljr/core.clj")
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core)\n\n|")))

  (it "turns the underscores of the file name into dashes"
    (cljr-test-with-source-file
      (cljr-test-visit "src/cljr/string_utils.cljs")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.string-utils)\n\n")))

  (it "requires the tested namespace and clojure.test in a test file"
    (cljr-test-with-source-file
      (cljr-test-visit "test/cljr/core_test.clj")
      (expect (cljr-test-buffer-string t) :to-equal "(ns cljr.core-test
  (:require [cljr.core :as sut]
            [clojure.test :as t]))

|")))

  (it "finds the tested namespace of a test file with a prefix"
    (cljr-test-with-source-file
      (cljr-test-visit "test/cljr/foo_core.clj")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.foo-core
  (:require [cljr.core :as sut]
            [clojure.test :as t]))

")))

  (it "finds the tested namespace of a test file with another suffix"
    (cljr-test-with-source-file
      (cljr-test-visit "test/cljr/core_bar.clj")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core-bar
  (:require [cljr.core :as sut]
            [clojure.test :as t]))

")))

  ;; Pending: today there are two spaces after :require, as the one that
  ;; goes before the tested namespace is there without it too.
  (xit "only requires clojure.test when there's no namespace to test"
    (cljr-test-with-source-file
      (cljr-test-visit "test/cljr/other_test.clj")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.other-test
  (:require [clojure.test :as t]))

")))

  (it "names the tested namespace as the user wants it"
    (cljr-test-with-source-file
      (let ((cljr-clojure-test-namespace-under-test-alias "core"))
        (cljr-test-visit "test/cljr/core_test.clj"))
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core-test
  (:require [cljr.core :as core]
            [clojure.test :as t]))

")))

  (it "requires midje in a project that depends on it"
    (cljr-test-with-source-file
      (cljr-test-depend-on "midje")
      (cljr-test-visit "test/cljr/core_test.clj")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core-test
  (:require [cljr.core :as sut]
            [midje.sweet :as midje]))

")))

  (it "requires midje in a test file with the t_ prefix"
    (cljr-test-with-source-file
      (cljr-test-depend-on "midje")
      (cljr-test-visit "test/cljr/t_core.clj")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.t-core
  (:require [cljr.core :as sut]
            [midje.sweet :as midje]))

")))

  (it "requires expectations in a project that depends on it"
    (cljr-test-with-source-file
      (cljr-test-depend-on "expectations")
      (cljr-test-visit "test/cljr/core_test.clj")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core-test
  (:require [cljr.core :as sut]
            [expectations :as e]))

")))

  (it "requires the test library of both languages in a cljc file"
    (cljr-test-with-source-file
      (cljr-test-visit "test/cljr/core_test.cljc")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core-test
  (:require [cljr.core :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

")))

  (it "requires cljs.test in a cljs file"
    (cljr-test-with-source-file
      (cljr-test-visit "test/cljr/core_test.cljs")
      (expect (cljr-test-buffer-string) :to-equal "(ns cljr.core-test
  (:require [cljr.core :as sut]
            [cljs.test :as t :include-macros true]))

")))

  (it "leaves the buffer unmodified when it isn't a Clojure file"
    (cljr-test-with-source-file
      (cljr-test-visit "src/cljr/notes.txt")
      (expect major-mode :not :to-be 'clojure-mode)
      (expect (cljr-test-buffer-string) :to-equal "")
      (expect (buffer-modified-p) :to-be nil)))

  (it "inserts nothing when cljr-add-ns-to-blank-clj-files is off"
    (cljr-test-with-source-file
      (let ((cljr-add-ns-to-blank-clj-files nil))
        (cljr-test-visit "test/cljr/core_test.clj"))
      (expect (cljr-test-buffer-string) :to-equal ""))))

(describe "visiting a Clojure file that has something in it"
  (it "leaves it as it is"
    (cljr-test-with-project (:name "cljr"
                             :files '(("src/cljr/core.clj" . "(def x 1)\n")))
      (cljr-test-visit "src/cljr/core.clj")
      (expect (cljr-test-buffer-string) :to-equal "(def x 1)\n")
      (expect (buffer-modified-p) :to-be nil))))

;;; auto-ns-test.el ends here
