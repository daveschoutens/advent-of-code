(ns aoc.y2022.day03)

(defn priority [c]
  (if (<= 97 (int c) 122)
    (- (int c) 96)
    (- (int c) (- 64 26))))

(defn compartments [s]
  (map set (partition (/ (count s) 2) (seq s))))

(defn common-item [[a b]]
  (first (clojure.set/intersection a b)))

(defn parse-input [input]
(clojure.string/split-lines input))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day03")))
  ([input]
   (->> (parse-input input)
        (map compartments)
        (map common-item)
        (map priority)
        (reduce +))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day03")))
  ([input]
   (->> (parse-input input)
        (map (comp set seq))
        (partition 3)
        (map (partial apply clojure.set/intersection))
        (map first)
        (map priority)
        (reduce +))))

(comment
  (solve-1)
  (solve-2))
