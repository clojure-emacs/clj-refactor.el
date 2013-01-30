Feature: Rename a file, update namespaces

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/rename_me.clj"

  Scenario: Rename
    When I open file "tmp/src/cljr/rename_me.clj"
    And I start an action chain
    And I press "C-x C-r"
    And I press "C-3 M-b"
    And I press "C-2 M-d"
    And I type "renamed"
    And I execute the action chain
    Then the file should be named "tmp/src/cljr/renamed.clj"
    And I should see "(ns cljr.renamed)"
