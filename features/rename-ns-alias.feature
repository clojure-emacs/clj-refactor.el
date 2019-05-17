Feature: Rename ns alias

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: With :as
    When I insert:
    """
    (ns cljr.core
      (:require [my.lib :as lib]))

    (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

    (+ (lib/a 1) (b 2))
    """
    And I place the cursor before "bare"
    And I start an action chain
    And I press "C-! ra"
    And I type "lib"
    And I press "RET"
    And I type "foo"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [my.lib :as foo]))

    (def m #::foo{:kw 1, :n/kw 2, :_/bare 3, 0 4})

    (+ (foo/a 1) (b 2))
    """

  Scenario: Without :as
    When I insert:
    """
    (ns cljr.core
      (:require [my.lib :as lib]))

    (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

    (+ (lib/a 1) (b 2))
    """
    And I place the cursor before "bare"
    And I start an action chain
    And I press "C-! ra"
    And I type "foo"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    (ns cljr.core
      (:require [my.lib :as lib]))

    (def m #::lib{:kw 1, :n/kw 2, :_/bare 3, 0 4})

    (+ (lib/a 1) (b 2))
    """
