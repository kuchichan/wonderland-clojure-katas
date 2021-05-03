(ns magic-square.puzzle
  (:require [clojure.set]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn row-alignment [values]
  (into #{} (for [a values
                  b values
                  c values
                  :when (and (not (= a b))
                             (not (= b c))
                             (not (= a c))
                             (= 9.0 (+ a b c)))]
              (sort [a b c]))))

(defn all-elements-different [col1 col2]
  (empty? (clojure.set/intersection (set col1) (set col2))))

(defn elements-different-against-coll [colls coll]
  (every? true? (map (partial all-elements-different coll) colls)))

(defn remove-duplicates [coll]
  (reduce (fn [r x]
            (if (true? (elements-different-against-coll r x))
              (conj r x)
              r)) [(first coll)] (rest coll)))

(defn column-alignment [matrix]
  (into #{} (for [a (matrix 0)
                  b (matrix 1)
                  c (matrix 2)
                  :when (= 9.0 (+ a b c))]
              [a b c])))

(defn diagonal-alignment [matrix]
  (for [a matrix
        b matrix
        c matrix
        :when (and (not (= a b))
                   (= 9.0 (+ (a 0) (b 1) (c 2)))
                   (= 9.0 (+ (a 2) (b 1) (c 0))))]
    [a b c]))

(defn magic-square [values]
  (->> (row-alignment values)
       (remove-duplicates)
       (column-alignment)
       (remove-duplicates)
       (diagonal-alignment)
       (first)
       ))

(magic-square values)
