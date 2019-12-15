(def input "231832-767346")

(def password-range 
  (let [[start end] (map #(Integer/parseInt %) (clojure.string/split input #"-"))]
    (range start (inc end))))

(first password-range)
(last password-range)

(defn only-increasing? [password]
  (apply <= (map #(Integer/parseInt (str %)) (str password))))

(defn contains-double? [password]
  (not-empty (filter (partial apply =) (partition 2 1 (str password)))))

;; Solution 1
(count (filter (every-pred only-increasing? contains-double?) password-range))

(defn contains-strictly-double? [password]
  (contains? (into #{} (vals (frequencies (str password)))) 2))

;; Solution 2
(count (filter (every-pred only-increasing? contains-strictly-double?) password-range))