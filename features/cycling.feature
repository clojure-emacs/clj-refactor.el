Feature: Code Cycling

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Cycling Privacy (defn -> defn-)
    When I insert:
    """
    (defn add [a b]
      (+ a b))
    """
    And I press "C-! cp"
    Then I should see:
    """
    (defn- add [a b]
      (+ a b))
    """

  Scenario: Cycling Privacy (defn- -> defn)

    When I insert:
    """
    (defn- add [a b]
      (+ a b))
    """
    And I press "C-! cp"
    Then I should see:
    """
    (defn add [a b]
      (+ a b))
    """

  Scenario: Cycling Privacy (def -> def ^:private)
    When I insert:
    """
    (def config
      "docs"
      {:env "staging"})
    """
    And I press "C-! cp"
    Then I should see:
    """
    (def ^:private config
      "docs"
      {:env "staging"})
    """

  Scenario: Cycling Privacy (def ^:private- -> def)

    When I insert:
    """
    (def ^:private config
      "docs"
      {:env "staging"})
    """
    And I press "C-! cp"
    Then I should see:
    """
    (def config
      "docs"
      {:env "staging"})
    """