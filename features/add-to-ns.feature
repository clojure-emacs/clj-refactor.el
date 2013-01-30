Feature: Add to namespace

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I press "M-<"

  Scenario: Add require to namespace
    And I press "C-! ar"
    And I type "clojure.strings"
    And I press "TAB"
    And I type "s"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.strings :as s]))
    """

  Scenario: Add use to namespace
    Then the cursor should be at point "1"
    When I press "C-! au"
    And I type "clj-time.core"
    And I press "TAB"
    And I press "C-! au"
    And I press "TAB"
    And I type "clojure.strings"
    And I press "TAB"
    And I type "join"
    Then I should see:
    """
    (ns cljr.core
      (:use [clojure.strings :only (join)])
      (:use clj-time.core))
    """
