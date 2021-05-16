(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn count-distance [word-a word-b]
  (loop [a word-a
         b word-b
         result 0]
    (if (empty? a)
      result
      (if (= (first a) (first b))
        (recur (rest a) (rest b) result)
        (recur (rest a) (rest b) (inc result))))))

(defn is-neighbor? [word-a word-b]
  (and (= (count word-a) (count word-b))
       (= (count-distance word-a word-b) 1)))

(defn get-neighbors [word]
  (filter (partial is-neighbor? word) words))

(defn visited? [col v]
  (some #(= % v) col))

(defn doublets [word1 word2]
  (loop [queue [[word1 []]]
         visited []]
    (if (empty? queue)
      queue
      (let [[word route] (first queue)
            neighbors (filter (complement (partial visited? visited)) (get-neighbors word))]
        (if (= word word2)
          (conj route word)
          (recur (concat (rest queue) (map #(vector % (conj route word)) neighbors)) (conj visited word)))))))
