(ns aoc.y2022.day08)

(defn forest [input]
  (let [num-cols (clojure.string/index-of input "\n")
        digits (->> (clojure.string/replace input #"\n" "") seq (map (comp parse-long str)))]
    {:rows (mapv #(into [] %) (partition num-cols digits))
     :cols (mapv #(into [] %) (for [i (range num-cols)] (take-nth num-cols (drop i digits))))}))

(defn surroundings [{:keys [rows cols] :as forest} x y]
  (let [[left right] (split-at x (get rows y))
        [above below] (split-at y (get cols x))
        [[tree] right] (split-at 1 right)
        below (rest below)]
    {:tree tree :left left :right right :above above :below below}))

(defn is-visible [{:keys [tree left right above below]}]
  (->> [left right above below]
       (map #(apply (partial max -1) %))
       (not-every? (partial <= tree))))

(defn scenic-score [{:keys [tree left right above below]}]
  (->> [(reverse left) right (reverse above) below]
       (map (fn [x] (count (take (inc (count (take-while #(> tree %) x))) x))))
       (reduce *)))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day08")))
  ([input]
   (let [{:keys [rows cols] :as forest} (forest input)]
     (->> (for [x (range (count cols))
                y (range (count rows))]
            (is-visible (surroundings forest x y)))
          (filter identity)
          count))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day08")))
  ([input]
   (let [{:keys [rows cols] :as forest} (forest input)]
     (->> (for [x (range (count cols))
                y (range (count rows))]
            (scenic-score (surroundings forest x y)))
          (reduce max)))))

(comment
  (solve-1)
  (solve-2))
