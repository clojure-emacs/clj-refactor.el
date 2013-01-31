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
    And I press "TAB"
    And I press "C-! ar"
    And I type "clj-time.core"
    And I press "TAB"
    And I type "time"
    And I press "TAB"
    Then I should see:
    """
    (ns cljr.core
      (:require [clojure.strings :as s]
                [clj-time.core :as time]))
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
      (:use clj-time.core
            [clojure.strings :only (join)]))
    """
