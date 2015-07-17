Feature: Create Function from Example

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Create function from example, all words
    When I insert:
    """
    (defn some-fn []
      (foo bar baz))
    """
    And I place the cursor before "bar"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [bar baz]
      )

    (defn some-fn []
      (foo bar baz))
    """

   Scenario: Create function from example, line breaks
    When I insert:
    """
    (defn some-fn []
      (foo
        bar
        baz))
    """
    And I place the cursor before "bar"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [bar baz]
      )

    (defn some-fn []
      (foo
        bar
        baz))
    """

  Scenario: Create function from example, all sorts of parameters
    When I insert:
    """
    (defn some-fn []
      (foo
        "some string"
        bar
        {:keyword :value}
        (clojure.string/trim (:a some-map))
        baz))
    """
    And I place the cursor before "baz"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [arg0 bar arg2 arg3 baz]
      )

    (defn some-fn []
      (foo
        "some string"
        bar
        {:keyword :value}
        (clojure.string/trim (:a some-map))
        baz))
    """

  Scenario: Create function from example no params
    When I insert:
    """
    (defn some-fn []
      (foo))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo []
      )

    (defn some-fn []
      (foo))
    """

  Scenario: Create function from example using literals
    When I insert:
    """
    (defn some-fn []
      (foo 1 bar :key #{1 2}))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- foo [arg0 bar arg2 arg3]
      )

    (defn some-fn []
      (foo 1 bar :key #{1 2}))
    """

  Scenario: Create function from example with thread-first
    When I insert "(-> game (reveal-tile index))"
    And I place the cursor after "reveal"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- reveal-tile [game index]
      )

    (-> game (reveal-tile index))
    """

  Scenario: Create function from example with thread-last
    When I insert "(->> game (reveal-tile index))"
    And I place the cursor after "reveal"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- reveal-tile [index game]
      )

    (->> game (reveal-tile index))
    """

  Scenario: Create function from example, update-in
    When I insert "(update-in foo [:bar :baz] do-stuff)"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [baz]
      )

    (update-in foo [:bar :baz] do-stuff)
    """

  Scenario: Create function from example, map
    When I insert "(map do-stuff items (:oxen bar))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [item ox]
      )

    (map do-stuff items (:oxen bar))
    """

  Scenario: Create function from example, sort-by
    When I insert "(sort-by my-keyfn comp items)"
    And I place the cursor after "my"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- my-keyfn [item]
      )

    (sort-by my-keyfn comp items)
    """

  Scenario: Create function from example, keep-indexed
    When I insert "(keep-indexed do-stuff foos (:bars baz))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [index foo bar]
      )

    (keep-indexed do-stuff foos (:bars baz))
    """

  Scenario: Guess at param name, keyword
    When I insert "(do-stuff (:foo x) (:bar x))"
    And I place the cursor after "do"
    And I press "C-! fe"
    Then I should see:
    """
    (defn- do-stuff [foo bar]
      )

    (do-stuff (:foo x) (:bar x))
    """

  Scenario: Placeholders for args
    When I insert:
    """
    (defn some-fn []
      (foo 1 2))
    """
    And I place the cursor after "foo"
    And I press "C-! fe"
    And I type "one"
    And I press "<tab>"
    And I type "two"
    And I press "<tab>"
    And I type "body"
    Then I should see:
    """
    (defn- foo [one two]
      body)

    (defn some-fn []
      (foo 1 2))
    """
