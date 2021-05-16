(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn is-higher? [card-a card-b]
  (let [rank-a (.indexOf ranks (second card-a))
        rank-b (.indexOf ranks (second card-b))]
    (compare rank-a rank-b)))

(defn win-scenario [p1 p2 table]
  [(vec (concat (conj (vec (rest p1)) (first p2) (first p1)) table))
   (vec (drop 1 p2)) []])

(defn loose-scenario [p1 p2 table]
  [(vec (drop 1 p1))
   (vec (concat (conj (vec (rest p2)) (first p1) (first p2)) table))
   []])

(defn lack-of-card-scenario-p1 [p1 p2 table len]
  [[(first p1)]
   (vec (drop len p2))
   (concat table (take len p1) (take len p2))])

(defn lack-of-card-scenario-p2 [p1 p2 table len]
  [(vec (drop len p1))
   [(first p2)]
   (concat table (take len p1) (take len p2))])

(defn draw-scenario [p1 p2 table]
  (let [max-possible-drop (min 4 (count p1) (count p2))
        p1-dropped (vec (drop max-possible-drop p1))
        p2-dropped (vec (drop max-possible-drop p2))]
    ;; Scenario related to situation, when one of sides has not enough cards to play full draw   
    (if (empty? p1-dropped)
      (lack-of-card-scenario-p1 p1 p2 table (dec max-possible-drop)))
    (if (empty p2-dropped)
      (lack-of-card-scenario-p2 p1 p2 table (dec max-possible-drop)))
    [(vec (drop max-possible-drop p1))
     (vec (drop max-possible-drop p2))
     (concat table (take max-possible-drop p1) (take max-possible-drop p2))]))

(defn play-game [player1-cards player2-cards]
  (loop [player1 player1-cards
         player2 player2-cards
         table []]
    (if (empty? player1)
      [player2 player1]
      (if (empty? player2)
        [player1 player2]
        (case (is-higher? (first player1) (first player2))
          1 (let [[p1 p2 table] (win-scenario player1 player2 table)]
              (recur p1 p2 table))
          -1 (let [[p1 p2 table] (loose-scenario player1 player2 table)]
               (recur p1 p2 table))
          0 (let [[p1 p2 table] (draw-scenario player1 player2 table)]
              (recur p1 p2 table)))))))

(defn start-game []
  (let [[p1 p2]  (map vec (partition 26 (shuffle cards)))]
    (play-game p1 p2)))




