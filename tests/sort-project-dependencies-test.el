;;; sort-project-dependencies-test.el --- Specs for sorting the project's dependencies -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cljr-test-helpers)

(defun cljr-test-sort-dependencies (project-clj)
  "Sort the dependencies of a project whose project.clj holds PROJECT-CLJ.
Return what the buffer visiting the file holds afterwards."
  (cljr-test-refactor-file "project.clj" project-clj
    (cljr-sort-project-dependencies)))

(describe "cljr-sort-project-dependencies"
  (it "sorts every dependency vector in the file"
    (expect (cljr-test-sort-dependencies "(defproject refactor-nrepl \"0.1.0-SNAPSHOT\"
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :dependencies [[org.clojure/clojure \"1.5.1\"]
                 [org.clojure/tools.nrepl \"0.2.3\"]
                 [org.clojure/tools.analyzer \"0.5.0\"]
                 [org.clojure/tools.analyzer.jvm \"0.5.0\"]
                 [org.clojure/tools.namespace \"0.2.5\"]
                 [org.clojure/tools.reader \"0.8.5\"]
                 [clj-http \"0.9.2\"]]
  :profiles {:test {:dependencies [[print-foo \"0.5.3\"]]}
             :1.5 {:dependencies [[org.clojure/tools.analyzer.jvm \"0.5.0\"]
                                  [org.clojure/clojure \"1.5.1\"]
                                  [org.clojure/tools.namespace \"0.2.5\"]
                                  [org.clojure/tools.analyzer \"0.5.0\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})")
            :to-equal "(defproject refactor-nrepl \"0.1.0-SNAPSHOT\"
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :dependencies [[clj-http \"0.9.2\"]
                 [org.clojure/clojure \"1.5.1\"]
                 [org.clojure/tools.analyzer \"0.5.0\"]
                 [org.clojure/tools.analyzer.jvm \"0.5.0\"]
                 [org.clojure/tools.namespace \"0.2.5\"]
                 [org.clojure/tools.nrepl \"0.2.3\"]
                 [org.clojure/tools.reader \"0.8.5\"]]
  :profiles {:test {:dependencies [[print-foo \"0.5.3\"]]}
             :1.5 {:dependencies [[org.clojure/clojure \"1.5.1\"]
                                  [org.clojure/tools.analyzer \"0.5.0\"]
                                  [org.clojure/tools.analyzer.jvm \"0.5.0\"]
                                  [org.clojure/tools.namespace \"0.2.5\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})
"))

  (it "keeps the metadata on the dependencies it moves"
    (expect (cljr-test-sort-dependencies "(defproject refactor-nrepl VERSION
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :dependencies [[org.clojure/clojure \"1.5.1\"]
                 [org.clojure/tools.nrepl \"0.2.6\"]
                 [http-kit \"2.1.19\"]
                 [org.clojure/data.json \"0.2.5\"]
                 [instaparse \"1.3.4\"]
                 ^:source-dep [com.cemerick/pomegranate \"0.3.0\"]
                 ^:source-dep [org.clojure/tools.analyzer.jvm \"0.6.5\"]
                 ^:source-dep [org.clojure/tools.namespace \"0.2.7\"]
                 ^:source-dep [org.clojure/tools.reader \"0.8.12\"]]
  :plugins [[thomasa/mranderson \"0.2.2\"]]
  :filespecs [{:type :bytes :path \"refactor-nrepl/refactor-nrepl/project.clj\" :bytes ~(slurp \"project.clj\")}]
  :profiles {:test {:dependencies [[print-foo \"1.0.1\"]]}
             :1.5 {:dependencies [[org.clojure/clojure \"1.5.1\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :dependencies [[me.raynes/fs \"1.4.6\"]]
                   :resource-paths [\"test/resources\"
                                    \"resources/testproject/src\"]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})")
            :to-equal "(defproject refactor-nrepl VERSION
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :dependencies [^:source-dep [com.cemerick/pomegranate \"0.3.0\"]
                 [http-kit \"2.1.19\"]
                 [instaparse \"1.3.4\"]
                 [org.clojure/clojure \"1.5.1\"]
                 [org.clojure/data.json \"0.2.5\"]
                 ^:source-dep [org.clojure/tools.analyzer.jvm \"0.6.5\"]
                 ^:source-dep [org.clojure/tools.namespace \"0.2.7\"]
                 [org.clojure/tools.nrepl \"0.2.6\"]
                 ^:source-dep [org.clojure/tools.reader \"0.8.12\"]]
  :plugins [[thomasa/mranderson \"0.2.2\"]]
  :filespecs [{:type :bytes :path \"refactor-nrepl/refactor-nrepl/project.clj\" :bytes ~(slurp \"project.clj\")}]
  :profiles {:test {:dependencies [[print-foo \"1.0.1\"]]}
             :1.5 {:dependencies [[org.clojure/clojure \"1.5.1\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :dependencies [[me.raynes/fs \"1.4.6\"]]
                   :resource-paths [\"test/resources\"
                                    \"resources/testproject/src\"]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})
"))

  (it "moves the comments along with the dependency they belong to"
    (expect (cljr-test-sort-dependencies "(defproject refactor-nrepl VERSION
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :dependencies [[org.clojure/clojure \"1.5.1\"]
                 [org.clojure/tools.nrepl \"0.2.6\"]
                 [http-kit \"2.1.19\"]
                 [org.clojure/data.json \"0.2.5\"]
                 ;; instaparse is so cool I sometimes have to stop randoms on the street
                 ;; just to tell them about it!
                 [instaparse \"1.3.4\"] ; seriously
                 ;; this lib contains several amazibles too
                 ^:source-dep [com.cemerick/pomegranate \"0.3.0\"]
                 ^:source-dep [org.clojure/tools.analyzer.jvm \"0.6.5\"]
                 ^:source-dep [org.clojure/tools.namespace \"0.2.7\"]
                 ^:source-dep [org.clojure/tools.reader \"0.8.12\"]]
  :plugins [[thomasa/mranderson \"0.2.2\"]]
  :filespecs [{:type :bytes :path \"refactor-nrepl/refactor-nrepl/project.clj\" :bytes ~(slurp \"project.clj\")}]
  :profiles {:test {:dependencies [[print-foo \"1.0.1\"]]}
             :1.5 {:dependencies [[org.clojure/clojure \"1.5.1\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :dependencies [[me.raynes/fs \"1.4.6\"]]
                   :resource-paths [\"test/resources\"
                                    \"resources/testproject/src\"]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})")
            :to-equal "(defproject refactor-nrepl VERSION
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :dependencies [;; this lib contains several amazibles too
                 ^:source-dep [com.cemerick/pomegranate \"0.3.0\"]
                 [http-kit \"2.1.19\"]
                 ;; instaparse is so cool I sometimes have to stop randoms on the street
                 ;; just to tell them about it!
                 [instaparse \"1.3.4\"] ; seriously
                 [org.clojure/clojure \"1.5.1\"]
                 [org.clojure/data.json \"0.2.5\"]
                 ^:source-dep [org.clojure/tools.analyzer.jvm \"0.6.5\"]
                 ^:source-dep [org.clojure/tools.namespace \"0.2.7\"]
                 [org.clojure/tools.nrepl \"0.2.6\"]
                 ^:source-dep [org.clojure/tools.reader \"0.8.12\"]]
  :plugins [[thomasa/mranderson \"0.2.2\"]]
  :filespecs [{:type :bytes :path \"refactor-nrepl/refactor-nrepl/project.clj\" :bytes ~(slurp \"project.clj\")}]
  :profiles {:test {:dependencies [[print-foo \"1.0.1\"]]}
             :1.5 {:dependencies [[org.clojure/clojure \"1.5.1\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :dependencies [[me.raynes/fs \"1.4.6\"]]
                   :resource-paths [\"test/resources\"
                                    \"resources/testproject/src\"]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})
"))

  ;; The vector of #73, which also isn't indented the way Emacs would.
  (it "copes with dependencies that span several lines"
    (expect (cljr-test-sort-dependencies "(defproject refactor-nrepl VERSION
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :dependencies [[bigml/closchema \"0.6-SNAPSHOT\"]
                  [clj-nsca \"0.0.3\"]
                  [clj-yaml \"0.4.0\"]
                  [clj-time \"0.6.0\"]
                  [org.clojure/core.typed \"0.2.68\"]
                  [clojurewerkz/cassaforte \"1.3.0-beta8\"
                   :exclusions [org.slf4j/slf4j-api]]
                  [com.aerokode.conversions \"0.0.7\"]
                  [com.yammer.metrics/metrics-graphite \"2.2.0\"]
                  [com.paypal.sdk/rest-api-sdk \"0.7.1\"]
                  [com.paypal.sdk/paypal-core \"1.5.2\"]
                  [compojure \"1.1.6\"]
                  [crypto-random \"1.2.0\"]
                  [http-kit \"2.1.18\"]
                  [log4j/log4j \"1.2.17\"
                   :exclusions [javax.mail/mail
                                javax.jms/jms
                                com.sun.jmdk/jmxtools
                                com.sun.jmx/jmxri]]
                  [net.logstash.log4j/jsonevent-layout \"1.6\"]
                  [metrics-clojure \"1.0.1\"]
                  [metrics-clojure-ring \"1.0.1\"]
                  [org.apache.commons/commons-daemon \"1.0.9\"]
                  [org.apache.httpcomponents/httpclient \"4.3.3\"]
                  [org.clojure/clojure \"1.6.0\"]
                  [org.clojure/data.codec \"0.1.0\"]
                  [org.clojure/data.json \"0.2.4\"]
                  [org.clojure/tools.cli \"0.3.1\"]
                  [org.clojure/tools.logging \"0.2.6\"]
                  [org.clojure/tools.nrepl \"0.2.3\"]
                  [org.clojure/tools.trace \"0.7.8\"]
                  [org.slf4j/slf4j-log4j12 \"1.7.6\"
                   :exclusions [org.slf4j/slf4j-api]]
                  [peridot \"0.2.2\"]
                  [org.clojure/test.check \"0.5.7\"]
                  [net.authorize/anet-java-sdk \"1.8.0\"]
                  [cloverage \"1.0.4\"]
                  [ring \"1.3.0\"]
                  [ring-cors \"0.1.4\"]
                  [ring/ring-core \"1.3.0\"]
                  [ring/ring-json \"0.3.1\"]
                  [slingshot \"0.10.3\"]
                  [swindon \"0.0.1\"]]
  :plugins [[thomasa/mranderson \"0.2.2\"]]
  :filespecs [{:type :bytes :path \"refactor-nrepl/refactor-nrepl/project.clj\" :bytes ~(slurp \"project.clj\")}]
  :profiles {:test {:dependencies [[print-foo \"1.0.1\"]]}
             :1.5 {:dependencies [[org.clojure/clojure \"1.5.1\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :dependencies [[me.raynes/fs \"1.4.6\"]]
                   :resource-paths [\"test/resources\"
                                    \"resources/testproject/src\"]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})")
            :to-equal "(defproject refactor-nrepl VERSION
  :description \"nREPL middleware to support editor agnostic refactoring\"
  :url \"http://github.com/clojure-emacs/refactor-nrepl\"
  :license {:name \"Eclipse Public License\"
            :url \"http://www.eclipse.org/legal/epl-v10.html\"}
  :dependencies [[bigml/closchema \"0.6-SNAPSHOT\"]
                 [clj-nsca \"0.0.3\"]
                 [clj-time \"0.6.0\"]
                 [clj-yaml \"0.4.0\"]
                 [clojurewerkz/cassaforte \"1.3.0-beta8\"
                  :exclusions [org.slf4j/slf4j-api]]
                 [cloverage \"1.0.4\"]
                 [com.aerokode.conversions \"0.0.7\"]
                 [com.paypal.sdk/paypal-core \"1.5.2\"]
                 [com.paypal.sdk/rest-api-sdk \"0.7.1\"]
                 [com.yammer.metrics/metrics-graphite \"2.2.0\"]
                 [compojure \"1.1.6\"]
                 [crypto-random \"1.2.0\"]
                 [http-kit \"2.1.18\"]
                 [log4j/log4j \"1.2.17\"
                  :exclusions [javax.mail/mail
                               javax.jms/jms
                               com.sun.jmdk/jmxtools
                               com.sun.jmx/jmxri]]
                 [metrics-clojure \"1.0.1\"]
                 [metrics-clojure-ring \"1.0.1\"]
                 [net.authorize/anet-java-sdk \"1.8.0\"]
                 [net.logstash.log4j/jsonevent-layout \"1.6\"]
                 [org.apache.commons/commons-daemon \"1.0.9\"]
                 [org.apache.httpcomponents/httpclient \"4.3.3\"]
                 [org.clojure/clojure \"1.6.0\"]
                 [org.clojure/core.typed \"0.2.68\"]
                 [org.clojure/data.codec \"0.1.0\"]
                 [org.clojure/data.json \"0.2.4\"]
                 [org.clojure/test.check \"0.5.7\"]
                 [org.clojure/tools.cli \"0.3.1\"]
                 [org.clojure/tools.logging \"0.2.6\"]
                 [org.clojure/tools.nrepl \"0.2.3\"]
                 [org.clojure/tools.trace \"0.7.8\"]
                 [org.slf4j/slf4j-log4j12 \"1.7.6\"
                  :exclusions [org.slf4j/slf4j-api]]
                 [peridot \"0.2.2\"]
                 [ring \"1.3.0\"]
                 [ring-cors \"0.1.4\"]
                 [ring/ring-core \"1.3.0\"]
                 [ring/ring-json \"0.3.1\"]
                 [slingshot \"0.10.3\"]
                 [swindon \"0.0.1\"]]
  :plugins [[thomasa/mranderson \"0.2.2\"]]
  :filespecs [{:type :bytes :path \"refactor-nrepl/refactor-nrepl/project.clj\" :bytes ~(slurp \"project.clj\")}]
  :profiles {:test {:dependencies [[print-foo \"1.0.1\"]]}
             :1.5 {:dependencies [[org.clojure/clojure \"1.5.1\"]]}
             :1.6 {:dependencies [[org.clojure/clojure \"1.6.0\"]]}
             :1.7 {:dependencies [[org.clojure/clojure \"1.7.0-master-SNAPSHOT\"]]}
             :dev {:plugins [[jonase/eastwood \"0.1.4\"]]
                   :dependencies [[me.raynes/fs \"1.4.6\"]]
                   :resource-paths [\"test/resources\"
                                    \"resources/testproject/src\"]
                   :repositories [[\"snapshots\" \"http://oss.sonatype.org/content/repositories/snapshots\"]]}})
"))

  (it "leaves sorted dependencies alone"
    (let ((sorted "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[clj-http \"0.9.2\"]
                 [org.clojure/clojure \"1.9.0\"]])
"))
      (expect (cljr-test-sort-dependencies sorted) :to-equal sorted)))

  (it "saves the project file"
    (cljr-test-with-project (:name "cljr")
      (cljr-test-visit "project.clj" "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.0\"]
                 [clj-http \"0.9.2\"]])")
      (cljr-sort-project-dependencies)
      (expect (buffer-modified-p) :to-be nil)
      (expect (cljr-test-file-string "project.clj" 'on-disk)
              :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[clj-http \"0.9.2\"]
                 [org.clojure/clojure \"1.9.0\"]])
")))

  (it "sorts a project file that isn't open, without leaving it open"
    (cljr-test-with-project (:name "cljr"
                             :files '(("project.clj" . "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[org.clojure/clojure \"1.9.0\"]
                 [clj-http \"0.9.2\"]])")
                                      ("src/cljr/core.clj" . "(ns cljr.core)")))
      (cljr-test-visit "src/cljr/core.clj")
      (cljr-sort-project-dependencies)
      (expect (get-file-buffer (cljr-test-file "project.clj")) :to-be nil)
      (expect (cljr-test-file-string "project.clj")
              :to-equal "(defproject cljr \"0.1.0-SNAPSHOT\"
  :dependencies [[clj-http \"0.9.2\"]
                 [org.clojure/clojure \"1.9.0\"]])
")))

  (it "refuses to sort a deps.edn"
    (cljr-test-with-project (:type 'deps)
      (cljr-test-visit "deps.edn")
      (let ((before (cljr-test-buffer-string)))
        (expect (cljr-sort-project-dependencies) :to-throw 'user-error)
        (expect (cljr-test-buffer-string) :to-equal before)))))

;;; sort-project-dependencies-test.el ends here
