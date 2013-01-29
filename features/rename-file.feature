Feature: Rename a file, update namespaces

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/rename_me.clj"

  Scenario: Rename
    When I open file "tmp/src/cljr/rename_me.clj"
    And I press "C-x C-r"
    Then I should see "New name: "
