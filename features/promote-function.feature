Feature: Tests for some minor features

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Promote fn to defn
    When I insert:
    """
    (map (fn [sym] (-> sym (str "!") symbol)) '[aww yeah])
    """
    And I place the cursor before "symbol"
    And I start an action chain
    And I press "C-! pf"
    And I type "shout-it!"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (defn shout-it!
      [sym]
      (-> sym (str "!") symbol))

    (map shout-it! '[aww yeah])
    """
