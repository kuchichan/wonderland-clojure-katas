(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "queens are higher rank than jacks"
    (is (= 1 (is-higher? [:spade :queen] [:spade :jack]))))
  (testing "kings are higher rank than queens"
    (is (= 1 (is-higher? [:spade :king] [:spade :queen])))
    (testing "aces are higher rank than kings"))
  (is (= 1 (is-higher? [:heart :ace] [:club :king]))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (= [] (second (start-game))))))

