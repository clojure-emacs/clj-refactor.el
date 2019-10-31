Feature: Magic uses

  Background:
    Given I have a project "cljr" in "tmp"
    And I add "my.prelude" to cljr-magic-use-namespaces
    And I add "my.http-client" to cljr-magic-use-namespaces
    When I have a clojure-file "tmp/src/cljr/core.clj"

  Scenario: Referrals are inserted automagically
    When I open file "tmp/src/cljr/core.clj"
    Then I should see:
    """
    (ns cljr.core
      (:require [my.http-client :refer :all]
                [my.prelude :refer :all]))
    """
