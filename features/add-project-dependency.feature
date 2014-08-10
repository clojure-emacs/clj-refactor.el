Feature: Add project dependency

  Background:
    Given I have a project "cljr" in "tmp"
    And I open file "tmp/project.clj"
    And I run cider-jack-in
    And the artifact cache is populated

  Scenario: Add prismatic/schema 0.2.2 to project dependency
    When I open file "tmp/project.clj"
    And I start an action chain
    And I press "C-! ap"
    And I type "prismatic/schema"
    And I press "RET"
    And I type "0.2.2"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (defproject cljr "0.1.0-SNAPSHOT"
    :dependencies [[org.clojure/clojure "1.6.0"]
                   [prismatic/schema "0.2.2"]]
    :plugins [[refactor-nrepl "0.1.0-SNAPSHOT"]])
    """