;;; project-dependencies-test.el --- Specs for adding and updating project dependencies -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defmacro cljr-test-with-build-file (type &rest body)
  "Evaluate BODY in the build file of a project of TYPE; return its text.
TYPE is `lein' or `deps'.  The file has a single dependency, on
Clojure 1.9.0, and nothing gets hotloaded."
  (declare (indent 1) (debug t))
  `(cljr-test-with-project (:name "cljr" :type ,type)
     (cljr-test-visit (if (eq ,type 'deps) "deps.edn" "project.clj"))
     (let ((cljr-hotload-dependencies nil))
       ,@body)
     (cljr-test-buffer-string)))

(describe "cljr--add-project-dependency"
  (describe "in a Leiningen project"
    (it "adds the dependency after the last one"
      (expect (cljr-test-with-build-file 'lein
                (cljr--add-project-dependency "org.clojure/tools.namespace" "0.3.0-alpha4"))
              :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.0\"]
                 [org.clojure/tools.namespace \"0.3.0-alpha4\"]])
"))

    (it "saves the project file"
      (cljr-test-with-build-file 'lein
        (cljr--add-project-dependency "hiccup" "2.0.0-alpha2")
        (expect (buffer-modified-p) :to-be nil)
        (expect (cljr-test-file-string "project.clj" 'on-disk)
                :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.0\"]
                 [hiccup \"2.0.0-alpha2\"]])
")))

    (it "changes a project file that isn't open, without leaving it open"
      (cljr-test-with-project (:name "cljr"
                               :files '(("src/cljr/core.clj" . "(ns cljr.core)")))
        (cljr-test-visit "src/cljr/core.clj")
        (let ((cljr-hotload-dependencies nil))
          (cljr--add-project-dependency "hiccup" "2.0.0-alpha2"))
        (expect (get-file-buffer (cljr-test-file "project.clj")) :to-be nil)
        (expect (cljr-test-file-string "project.clj")
                :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.0\"]
                 [hiccup \"2.0.0-alpha2\"]])
")))

    (it "hotloads the dependency it added when asked to"
      (let (hotloaded)
        (spy-on 'cljr-hotload-dependency
                :and-call-fake (lambda () (setq hotloaded (cljr--dependency-at-point))))
        (cljr-test-with-build-file 'lein
          (let ((cljr-hotload-dependencies t))
            (cljr--add-project-dependency "hiccup" "2.0.0-alpha2")))
        (expect hotloaded :to-equal "[hiccup \"2.0.0-alpha2\"]")))

    (it "complains about a project file without dependencies"
      (cljr-test-with-project (:name "cljr")
        (cljr-test-visit "project.clj" "(defproject cljr \"0.1.0-SNAPSHOT\")")
        (let ((cljr-hotload-dependencies nil))
          (expect (cljr--add-project-dependency "hiccup" "2.0.0-alpha2")
                  :to-throw 'user-error))
        (expect (cljr-test-buffer-string)
                :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\")"))))

  (describe "in a deps.edn project"
    (it "adds the dependency after the last one"
      (expect (cljr-test-with-build-file 'deps
                (cljr--add-project-dependency "org.clojure/tools.namespace" "0.3.0-alpha4"))
              :to-equal "{:paths [\"src\" \"resources\"]
 :deps {org.clojure/clojure {:mvn/version \"1.9.0\"}
        org.clojure/tools.namespace {:mvn/version \"0.3.0-alpha4\"}}}
"))

    (it "uses the name of an artifact without a group as its group"
      (expect (cljr-test-with-build-file 'deps
                (cljr--add-project-dependency "hiccup" "2.0.0-alpha2"))
              :to-equal "{:paths [\"src\" \"resources\"]
 :deps {org.clojure/clojure {:mvn/version \"1.9.0\"}
        hiccup/hiccup {:mvn/version \"2.0.0-alpha2\"}}}
"))

    (it "hotloads the dependency it added when asked to"
      (let (hotloaded)
        (spy-on 'cljr-hotload-dependency
                :and-call-fake (lambda () (setq hotloaded (cljr--dependency-at-point))))
        (cljr-test-with-build-file 'deps
          (let ((cljr-hotload-dependencies t))
            (cljr--add-project-dependency "hiccup" "2.0.0-alpha2")))
        (expect hotloaded :to-equal "[hiccup/hiccup \"2.0.0-alpha2\"]")))

    (it "complains about a deps.edn without dependencies"
      (cljr-test-with-project (:type 'deps)
        (cljr-test-visit "deps.edn" "{:paths [\"src\"]}")
        (let ((cljr-hotload-dependencies nil))
          (expect (cljr--add-project-dependency "hiccup" "2.0.0-alpha2")
                  :to-throw 'user-error))
        (expect (cljr-test-buffer-string) :to-equal "{:paths [\"src\"]}")))))

(describe "cljr-update-project-dependency"
  (before-each
    (cljr-test-without-middleware))

  (it "changes the version of the Leiningen dependency at point"
    (expect (cljr-test-with-build-file 'lein
              (cljr-test-goto "[org.clojure/clojure")
              (cljr-update-project-dependency "1.9.1"))
            :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.1\"]])"))

  (it "changes the version of the deps.edn dependency at point"
    (expect (cljr-test-with-build-file 'deps
              (cljr-test-goto "org.clojure/clojure")
              (cljr-update-project-dependency "1.9.1"))
            :to-equal "{:paths [\"src\" \"resources\"]
 :deps {org.clojure/clojure {:mvn/version \"1.9.1\"}}}"))

  (it "leaves point in front of the dependency"
    (cljr-test-with-build-file 'lein
      (cljr-test-goto "[org.clojure/clojure")
      (cljr-update-project-dependency "1.9.1")
      (expect (looking-at-p (regexp-quote "[org.clojure/clojure \"1.9.1\"]"))
              :to-be-truthy)))

  (it "doesn't need the middleware when it's given the version"
    (cljr-test-with-build-file 'lein
      (cljr-test-goto "[org.clojure/clojure")
      (cljr-update-project-dependency "1.9.1"))
    (expect 'cljr--ensure-op-supported :not :to-have-been-called))

  (it "asks the middleware for the versions when it isn't given one"
    (spy-on 'cljr--get-versions-from-middleware :and-return-value '("1.10.3" "1.9.1"))
    (expect (cljr-test-with-build-file 'lein
              (cljr-test-goto "[org.clojure/clojure")
              (cljr-test-call #'cljr-update-project-dependency "1.9.1"))
            :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.1\"]])")
    (expect 'cljr--get-versions-from-middleware
            :to-have-been-called-with "org.clojure/clojure"))

  (it "hotloads the updated dependency when asked to"
    (let (hotloaded)
      (spy-on 'cljr-hotload-dependency
              :and-call-fake (lambda () (setq hotloaded (cljr--dependency-at-point))))
      (cljr-test-with-build-file 'deps
        (cljr-test-goto "org.clojure/clojure")
        (let ((cljr-hotload-dependencies t))
          (cljr-update-project-dependency "1.9.1")))
      (expect hotloaded :to-equal "[org.clojure/clojure \"1.9.1\"]")))

  (it "complains when point isn't in front of a dependency"
    (expect (cljr-test-with-build-file 'lein
              (cljr-test-goto "org.clojure/clojure")
              (expect (cljr-update-project-dependency "1.9.1")
                      :to-throw 'user-error))
            :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.0\"]])")))

(describe "cljr--dependency-at-point"
  (it "finds the Leiningen dependency point is inside of"
    (cljr-test-with-build-file 'lein
      (cljr-test-goto "1.9.0")
      (expect (cljr--dependency-at-point)
              :to-equal "[org.clojure/clojure \"1.9.0\"]")))

  (it "turns a deps.edn dependency into a Leiningen one"
    (cljr-test-with-build-file 'deps
      (cljr-test-goto "org.clojure/clojure")
      (expect (cljr--dependency-at-point)
              :to-equal "[org.clojure/clojure \"1.9.0\"]")))

  (it "finds nothing away from the dependencies"
    (cljr-test-with-build-file 'lein
      (cljr-test-goto "0.1.0-SNAPSHOT")
      (expect (cljr--dependency-at-point) :to-be nil))))

;;; project-dependencies-test.el ends here
