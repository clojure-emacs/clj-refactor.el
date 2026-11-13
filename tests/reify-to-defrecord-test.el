;;; reify-to-defrecord-test.el --- Specs for cljr-reify-to-defrecord -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(describe "cljr-reify-to-defrecord"
  (it "replaces the reify with a call to the constructor of a new record"
    (expect (cljr-test-refactor "(defn create-runner []
  (reify Runnable
    (run [|this] :running)))"
              (cljr-test-call #'cljr-reify-to-defrecord "Runner"))
            :to-equal "(defrecord Runner []
  Runnable
  (run [this] :running))

(defn create-runner []
  (Runner.))"))

  (it "handles a reify that has the interface on a line of its own"
    (expect (cljr-test-refactor "(.listFiles (java.io.File. \".\")
            (reify
              java.io.FileFilter
              (accept [|this f]
                (.isDirectory f))))"
              (cljr-test-call #'cljr-reify-to-defrecord "FileFilteringFoo"))
            :to-equal "(defrecord FileFilteringFoo []
  java.io.FileFilter
  (accept [this f]
    (.isDirectory f)))

(.listFiles (java.io.File. \".\")
            (FileFilteringFoo.))"))

  (it "keeps all the interfaces the reify implements"
    (expect (cljr-test-refactor "(.listFiles (java.io.File. \".\")
            (reify
              java.io.FileFilter
              (accept [|this f]
                (.isDirectory f))
              java.lang.Object
              (toString [this] (println \"'sup?\"))))"
              (cljr-test-call #'cljr-reify-to-defrecord "FileFilteringFoo"))
            :to-equal "(defrecord FileFilteringFoo []
  java.io.FileFilter
  (accept [this f]
    (.isDirectory f))
  java.lang.Object
  (toString [this] (println \"'sup?\")))

(.listFiles (java.io.File. \".\")
            (FileFilteringFoo.))"))

  (it "puts the record after the ns form"
    (expect (cljr-test-refactor "(ns cljr.core)

(defn create-runner []
  (reify Runnable
    (run [|this] :running)))"
              (cljr-test-call #'cljr-reify-to-defrecord "Runner"))
            :to-equal "(ns cljr.core)

(defrecord Runner []
  Runnable
  (run [this] :running))

(defn create-runner []
  (Runner.))"))

  (it "works with point on the reify itself"
    (expect (cljr-test-refactor "(defn create-runner []
  |(reify Runnable
    (run [this] :running)))"
              (cljr-test-call #'cljr-reify-to-defrecord "Runner"))
            :to-equal "(defrecord Runner []
  Runnable
  (run [this] :running))

(defn create-runner []
  (Runner.))"))

  (it "leaves point in the call to the constructor"
    (expect (cljr-test-with-buffer "(defn create-runner []
  (reify Runnable
    (run [|this] :running)))"
              (cljr-test-call #'cljr-reify-to-defrecord "Runner")
              (cljr-test-buffer-string t))
            :to-equal "(defrecord Runner []
  Runnable
  (run [this] :running))

(defn create-runner []
  (Runner.|))"))

  (it "complains, and stays put, when point isn't in a reify"
    (cljr-test-with-buffer "(defn create-runner []
  (proxy [Runnable] []
    (run [] |:running)))"
      (expect (cljr-reify-to-defrecord) :to-throw 'error)
      (expect (cljr-test-buffer-string t)
              :to-equal "(defn create-runner []
  (proxy [Runnable] []
    (run [] |:running)))"))))

;;; reify-to-defrecord-test.el ends here
