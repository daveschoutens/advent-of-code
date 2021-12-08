(ns aoc.y2021.day03
  (:require [clojure.string :as str]))

(def raw-input (str/split-lines (slurp "input/2021day03")))
(def raw-input ["00100"
                "11110"
                "10110"
                "10111"
                "10101"
                "01111"
                "00111"
                "11100"
                "10000"
                "11001"
                "00010"
                "01010"])
(def input-bit-length ((comp count first) raw-input))
(def input (map #(Integer/parseInt % 2) raw-input))

(defn gamma-bit [input n]
  (<= (/ (count input) 2)
      (->> input
           (map #(bit-test % n))
           (filter identity)
           count)))

(defn gamma-value [input len]
  (->> (range len)
       (filter #(gamma-bit input %))
       (reduce #(bit-set %1 %2) 0)))

(defn epsilon-value [input len]
  (->> (range len)
       (filter #(not (gamma-bit input %)))
       (reduce #(bit-set %1 %2) 0)))

(defn solve-1 [input len]
  (let [gamma (gamma-value input len)
        epsilon (epsilon-value input len)]
    (* gamma epsilon)))

(defn solve-2 [input len]
  (let [gamma (gamma-value input len)
        epsilon (epsilon-value input len)
        O2-gen-rating ()
        ]))

(solve-1 input input-bit-length)

(defn recur-solve [bit-determinator-fn input len]
  (loop [input input
         n (dec len)]
    (let [value (bit-determinator-fn input n)
          remaining-input (filter #(= value (bit-test % n)) input)]
      (println {:input (map #(Integer/toBinaryString %) input) :value value :n n})
      (if (= 1 (count input)) (first input)
          (if (< n 0) nil
              (recur remaining-input (dec n)))))))

(defn solve-2 [input len]
  (let [o2-gen-rating (recur-solve gamma-bit input input-bit-length)
        co2-scrub-rating (recur-solve #(not (gamma-bit %1 %2)) input input-bit-length)]
    (* o2-gen-rating co2-scrub-rating)))

(solve-2 input input-bit-length)
