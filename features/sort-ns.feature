Feature: Sort ns form

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I press "M-<"

  Scenario: Sort ns
    When I insert:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.test
            clojure.test
            clojure.string)
      (:require [foo.bar :refer :all]
                [clj-time.core :as clj-time])
      (:import (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]
               (java.nio.charset Charset)))
    """
    And I place the cursor before "Calendar"
    And I press "C-! sn"
    Then The buffer should be modified
    Then I should see:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.string
            clojure.test)
      (:require [clj-time.core :as clj-time]
                [foo.bar :refer :all])
      (:import (java.nio.charset Charset)
               (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]))
    """
  Scenario: Sort ns that already sorted
    When I insert:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.string
            clojure.test)
      (:require [clj-time.core :as clj-time]
                [foo.bar :refer :all])
      (:import (java.nio.charset Charset)
               (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]))
    """
    And I place the cursor before "Calendar"
    And I save the file
    And I press "C-! sn"
    Then The buffer should not be modified
    Then I should see:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.string
            clojure.test)
      (:require [clj-time.core :as clj-time]
                [foo.bar :refer :all])
      (:import (java.nio.charset Charset)
               (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]))
    """

  Scenario: no ns form
    When I insert:
    """
    {:config :file}
    """
    And I place the cursor before "file"
    And I press "C-! sn"
    Then I should see:
    """
    {:config :file}
    """
  Scenario: regression 1 (:use in file doesn't confuse)
    When I insert:
    """
    (ns furtive.runtime.session.bucket
      (:require [foo.bar :refer :all]
                [clj-time.core :as clj-time]))

      (defn foo [x]
        (:user-agent x))
    """
    And I place the cursor before "foo"
    And I press "C-! sn"
    Then I should see:
    """
    (ns furtive.runtime.session.bucket
      (:require [clj-time.core :as clj-time]
                [foo.bar :refer :all]))

      (defn foo [x]
        (:user-agent x))
    """

  Scenario: Sort ns with length comparator
    When I insert:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.test
            clojure.test
            clojure.string)
      (:require [foo.bar :refer :all]
                [clj-time.core :as clj-time])
      (:import (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]
               (java.nio.charset Charset)))
    """
    And I set sort comparator to string length
    And I place the cursor before "Calendar"
    And I press "C-! sn"
    Then I should see:
    """
    (ns ^{:doc "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
            eiusmod tempor incididunt ut labore (et dolore magna aliqua)."}
      furtive.runtime.session.bucket
      (:use clojure.string
            clojure.test)
      (:require [clj-time.core :as clj-time]
                [foo.bar :refer :all])
      (:import (java.security MessageDigest)
               (java.nio.charset Charset)
               [org.joda.time DateTime]
               java.util.Calendar))
    """

  Scenario: Sort ns with semantic comparator
    When I insert:
    """
    (ns foo.bar.baz.goo
      (:require [clj-time.bla :as bla]
                [foo.bar.baz.bam :refer :all]
                [foo.bar.async :refer :all]
                [foo [bar.goo :refer :all] [baz :refer :all]]
                [async.funkage.core :as afc]
                [clj-time.core :as clj-time]
                [foo.async :refer :all])
      (:import (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]
               (java.nio.charset Charset)))
    """
    And I set sort comparator to semantic
    And I place the cursor before "Calendar"
    And I press "C-! sn"
    Then I should see:
    """
    (ns foo.bar.baz.goo
      (:require [foo.bar.baz.bam :refer :all]
                [foo.bar.async :refer :all]
                [foo.async :refer :all]
                [foo [bar.goo :refer :all] [baz :refer :all]]
                [async.funkage.core :as afc]
                [clj-time.bla :as bla]
                [clj-time.core :as clj-time])
      (:import (java.nio.charset Charset)
               (java.security MessageDigest)
               java.util.Calendar
               [org.joda.time DateTime]))
    """

  Scenario: Sort ignores commented out section
    When I insert:
    """
    (ns binomial.core
      (:gen-class)
      (:require [clojure.string :as   s]
                [clojure.set :as set])
      ;; (:import mage.interfaces.MageClient
      ;;          mage.interfaces.MageServer)
      )

    (walk)
    """
    And I place the cursor before "walk"
    And I press "C-! sn"
    Then I should see:
    """
    (ns binomial.core
      (:gen-class)
      (:require [clojure.set :as set]
                [clojure.string :as   s])
      ;; (:import mage.interfaces.MageClient
      ;;          mage.interfaces.MageServer)
      )

    (walk)
    """

  Scenario: Sort works with :require-macros
    When I insert:
    """
    (ns fancy.ns
      (:require-macros [my.macros :as  my]
                       [his.macros :as his])
      (:require [clojure.string :as   s]
                [clojure.set :as set]))

    (bar)
    """
    And I place the cursor before "bar"
    And I press "C-! sn"
    Then I should see:
    """
    (ns fancy.ns
      (:require-macros [his.macros :as his]
                       [my.macros :as  my])
      (:require [clojure.set :as set]
                [clojure.string :as   s]))

    (bar)
    """
