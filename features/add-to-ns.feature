Feature: Add to namespace

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"

  Scenario: Add require to namespace
    And I press "C-! ar"
    And I type "clojure.strings"
    And I press "C-x C-s"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.strings]))
    """

  Scenario: Add use to namespace
    And I press "C-! au"
    And I type "clojure.strings"
    And I press "C-x C-s"
    Then I should see:
    """
    (ns cljr.core
      (:use [clojure.strings :only ()]))
    """
