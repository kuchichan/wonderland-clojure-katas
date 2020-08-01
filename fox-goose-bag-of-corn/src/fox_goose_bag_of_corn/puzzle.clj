(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]))

(def start-pos [#{:you :fox :goose :corn} #{:boat} #{}])
(def end-pos [#{} #{:boat} #{:you :fox :goose :corn}])
(def forbidden-states [#{:fox :goose}
                       #{:goose :corn}])

(defn get-n-pop [col]
  ""
  (map
   (fn [itm]
     (if (not (= itm :you))
       [ #{:you itm } (disj col :you itm)]
       [ #{itm} (disj col itm)]
       ))
   col)
  )

(defn visited?
  [v col]
  (some #(= % v) col)
)

(defn possible-trips [state]
  "Depending on what side you are add & remove from side to side"
  (let [[l b r] state]
      (if (contains? l :you)
        (map (fn [x]
               [(last x) b (clojure.set/union r (first x))])
             (get-n-pop l))
        (map (fn [x]
               [(clojure.set/union l (first x)) b (last x)])
             (get-n-pop r))
        ))
  )

(defn allowed? [col]
  "Check that given state is allowed"
  (not (some (partial contains? (set forbidden-states)) col))
  )
 
(defn allowed-trips [state]
  (filter allowed? (possible-trips state)))


(defn get-goal [source goal]
  "BFS with route follow up"
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [source []])
         visited []]
    (if (= (first (peek visited)) goal) (conj (second (peek queue)) goal)
        (let [[trip route :as edge]       (peek queue)
              neighbors                   (map #(vector % (conj route trip)) (allowed-trips trip))
              not-visited                 (filter (complement #(visited? (first %) (map first visited))) neighbors)
              new-queue                   (apply conj (pop queue) not-visited)]
          (if (visited? trip (map first visited))
            (recur new-queue visited)
            (recur new-queue (conj visited edge)))))))


(defn river-crossing-plan []
  (get-goal start-pos end-pos)
  )

(river-crossing-plan)

