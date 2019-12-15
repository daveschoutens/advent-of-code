(ns aoc.y2019.day04)

(defn password-range [input] 
  (let [[start end] (map #(Integer/parseInt %) (clojure.string/split input #"-"))]
    (range start (inc end))))

(defn only-increasing? [password]
  (apply <= (map #(Integer/parseInt (str %)) (str password))))

(defn contains-double? [password]
  (not-empty (filter (partial apply =) (partition 2 1 (str password)))))

(defn solve-1
  ([] (solve-1 "231832-767346"))
  ([input] (->> input
                password-range
                (filter (every-pred only-increasing? contains-double?))
                count)))

(defn contains-strictly-double? [password]
  (contains? (into #{} (vals (frequencies (str password)))) 2))

(defn solve-2
  ([] (solve-2 "231832-767346"))
  ([input] (->> input
                password-range
                (filter (every-pred only-increasing? contains-strictly-double?))
                count)))