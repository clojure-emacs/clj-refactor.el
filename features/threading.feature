Feature: Threading and unwinding of macros

  Background:
    Given I have a project "cljr" in "tmp"
    And I have a clojure-file "tmp/src/cljr/core.clj"
    And I open file "tmp/src/cljr/core.clj"
    And I clear the buffer

  Scenario: Thread first, part 1
    When I insert "(-> (dissoc (assoc {} :key "value") :lock))"
    And I press "C-! th"
    Then I should see:
    """
    (-> (assoc {} :key "value")
        (dissoc :lock))
    """

  Scenario: Thread first, part 2
    When I insert "(-> (dissoc (assoc {} :key "value") :lock))"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """

  Scenario: Thread first, part 3 - don't thread maps and stuff
    When I insert "(-> (dissoc (assoc {} :key "value") :lock))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """

  Scenario: Thread first, part 4 - don't thread last one
    When I insert "(-> (dissoc (assoc (get-a-map) :key "value") :lock))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (-> (get-a-map)
        (assoc :key "value")
        (dissoc :lock))
    """


  Scenario: Unwind first, part 1
    When I insert:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (-> (assoc {} :key "value")
        (dissoc :lock))
    """

  Scenario: Unwind first, part 2
    When I insert:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(-> (dissoc (assoc {} :key "value") :lock))"

  Scenario: Unwind first, part 3 - jump out of threading
    When I insert:
    """
    (-> {}
        (assoc :key "value")
        (dissoc :lock))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(dissoc (assoc {} :key "value") :lock)"
    And I should not see "->"

  Scenario: Thread last, part 1
    When I insert "(->> (map square (filter even? [1 2 3 4 5])))"
    And I press "C-! th"
    Then I should see:
    """
    (->> (filter even? [1 2 3 4 5])
         (map square))
    """

  Scenario: Thread last, part 2
    When I insert "(->> (map square (filter even? [1 2 3 4 5])))"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """

  Scenario: Thread last, part 3 - don't thread vectors and stuff
    When I insert "(->> (map square (filter even? [1 2 3 4 5])))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """

  Scenario: Thread last, part 4 - don't thread last one
    When I insert "(->> (map square (filter even? (get-a-list))))"
    And I press "C-! th"
    And I press "C-! th"
    And I press "C-! th"
    Then I should see:
    """
    (->> (get-a-list)
         (filter even?)
         (map square))
    """

  Scenario: Unwind last, part 1
    When I insert:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (->> (filter even? [1 2 3 4 5])
         (map square))
    """

  Scenario: Unwind last, part 2
    When I insert:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(->> (map square (filter even? [1 2 3 4 5])))"

  Scenario: Unwind last, part 3 - jump out of threading
    When I insert:
    """
    (->> [1 2 3 4 5]
         (filter even?)
         (map square))
    """
    And I press "C-! uw"
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(map square (filter even? [1 2 3 4 5]))"
    And I should not see "->>"

  Scenario: Unwind function name, part 1
    When I insert:
    """
    (->> [1 2 3 4 5]
         sum
         square)
    """
    And I press "C-! uw"
    Then I should see:
    """
    (->> (sum [1 2 3 4 5])
         square)
    """

  Scenario: Unwind function name, part 2
    When I insert:
    """
    (-> [1 2 3 4 5]
        sum
        square)
    """
    And I press "C-! uw"
    And I press "C-! uw"
    Then I should see "(-> (square (sum [1 2 3 4 5])))"

  Scenario: Unwind, issue #6, part 1 - formatting
    When I insert:
    """
    (defn plus [a b]
      (-> a (+ b)))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (defn plus [a b]
      (-> (+ a b)))
    """

  Scenario: Unwind, issue #6, part 2 - formatting
    When I insert:
    """
    (defn plus [a b]
      (->> a (+ b)))
    """
    And I press "C-! uw"
    Then I should see:
    """
    (defn plus [a b]
      (->> (+ b a)))
    """
