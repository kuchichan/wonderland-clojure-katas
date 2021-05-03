(ns alphabet-cipher.coder)

(defn string-to-int-array [str]
  (->> (char-array str)
       (seq)
       (map int)
       (map (partial + -97))))

;; actually decoded ints and encoded ints could be a one function (+ / - difference)


(defn calculate-encoded-ints [keyword message]
  (->> (map string-to-int-array [message
                                 (take (count message) (cycle keyword))])
       (apply map +)
       (map #(mod % 26))
       (map (partial + 97))))

(defn calculate-decoded-ints [keyword message]
  (->> (map string-to-int-array [message
                                 (take (count message) (cycle keyword))])
       (apply map -)
       (map #(mod % 26))
       (map (partial + 97))))

(defn int-array-to-string [xs]
  (->> (map char xs)
       (apply str)))

(defn encode [keyword message]
  (->> (calculate-encoded-ints keyword message)
       (int-array-to-string)))

(defn decode [keyword message]
  (->> (calculate-decoded-ints keyword message)
       (int-array-to-string)))

(defn take-substrings [string]
  (map (partial subs string 0) (range 1 (-> string count inc))))

(defn decipher [cipher message]
  (first (get
          (->> (map string-to-int-array [cipher message])
               (apply map -)
               (map #(mod % 26))
               (map (partial + 97))
               (int-array-to-string)
               (take-substrings)
               (group-by #(= message (decode % cipher))))
          true)))
