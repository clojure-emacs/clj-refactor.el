Feature: Let bindings

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Introduce let
    When I insert:
    """
    {:status 200
     :body (find-body abc)}
    """
    And I place the cursor before "(find-body abc)"
    And I press "C-! il"
    And I type "body"
    Then I should see:
    """
    {:status 200
     :body (let [body (find-body abc)]
             body)}
    """

  Scenario: Expand let, part 1
    When I insert:
    """
    (defn handle-request
      {:status 200
       :body (let [body (find-body abc)]
               body)})
    """
    And I place the cursor before "(find-body abc)"
    And I press "C-! el"
    Then I should see:
    """
    (defn handle-request
      (let [body (find-body abc)]
        {:status 200
         :body body}))
    """

  Scenario: Expand let, part 2
    When I insert:
    """
    (defn handle-request
      (let [body (find-body abc)]
        {:status 200
         :body body}))
    """
    And I place the cursor before "(find-body abc)"
    And I press "C-! el"
    Then I should see:
    """
    (let [body (find-body abc)]
      (defn handle-request
        {:status 200
         :body body}))
    """
