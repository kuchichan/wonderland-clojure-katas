(ns wonderland-number.finder)

(def multipliers [2 3 4 5 6])

(def number-range (range 100000 999999))

(defn has-same-digits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn is-wonderland? [number]
  (every? true? (map (partial has-same-digits? number) (map (partial * number) multipliers))))

(is-wonderland? 100000)

(defn wonderland-number []
  (first (filter is-wonderland? number-range)))

(wonderland-number)

