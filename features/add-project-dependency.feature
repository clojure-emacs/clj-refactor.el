Feature: Add project dependencies

  Background:
    Given I have a project "cljr" in "tmp"
    And I open file "tmp/project.clj"
    And I clear the buffer

  Scenario: Add project dependency without sorting
    When I insert:
    """
    (defproject example-project "1.0.0"
      :description "Example project"
      :dependencies [[org.clojure/clojure "1.8.0"]
                     [clj-time "0.12.0"]]
      :main example-project.core)
    """
    And I don't want my project dependencies to be sorted automatically
    And I start an action chain
    And I press "C-! ap"
    And I type "com.github.bdesham/clj-plist"
    And I press "RET"
    And I type "0.10.0"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (defproject example-project "1.0.0"
      :description "Example project"
      :dependencies [[org.clojure/clojure "1.8.0"]
                     [clj-time "0.12.0"]
                     [com.github.bdesham/clj-plist "0.10.0"]]
      :main example-project.core)
    """

  Scenario: Add project dependency with sorting
    When I insert:
    """
    (defproject example-project "1.0.0"
      :description "Example project"
      :dependencies [[org.clojure/clojure "1.8.0"]
                     [clj-time "0.12.0"]]
      :main example-project.core)
    """
    And I want my project dependencies to be sorted automatically
    And I press "C-! ap"
    And I type "com.github.bdesham/clj-plist"
    And I press "RET"
    And I type "0.10.0"
    And I press "RET"
    Then I should see:
    """
    (defproject example-project "1.0.0"
      :description "Example project"
      :dependencies [[clj-time "0.12.0"]
                     [com.github.bdesham/clj-plist "0.10.0"]
                     [org.clojure/clojure "1.8.0"]]
      :main example-project.core)
    """
