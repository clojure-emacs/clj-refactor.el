Feature: Rename a file, update namespaces

  Scenario: Rename and update dependencies
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/dependency.clj"
    And I have a clojure-file "tmp/src/cljr/dependent_file.clj"

    When I open file "tmp/src/cljr/dependent_file.clj"
    And I press "M->"
    And I insert:
    """

    (cljr.dependency/abc 123)
    """
    And I press "C-x C-s"

    And I open file "tmp/src/cljr/dependency.clj"
    And I start an action chain
    And I press "C-x C-r"
    And I press "C-2 M-b"
    And I press "M-d"
    And I type "renamed"
    And I press "RET"
    And I press "y"
    And I execute the action chain

    When I open file "tmp/src/cljr/renamed.clj"
    Then I should see "(ns cljr.renamed)"

    When I open file "tmp/src/cljr/dependent_file.clj"
    Then I should see "(cljr.renamed/abc 123)"
