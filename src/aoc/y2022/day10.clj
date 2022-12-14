(ns aoc.y2022.day10)

(defn execute [program]
  (->>
   (reduce
    (fn [timestate instruction]
      (let [state (last timestate)
            [_ inst param] (re-matches #"(\w+) ?(.*)?" instruction)]
        (case inst
          "noop" (conj timestate state)
          "addx" (conj timestate state (+ state (parse-long param))))))
    [1]
    program)
   (map-indexed #(hash-map :x %2 :clock (inc %1)))))

(defn signal-strength [dump]
  (->> dump
       (drop 19)
       (take-nth 40)
       (map (fn [{:keys [x clock]}] (* x clock)))
       (reduce +)))

(defn draw-crt [dump]
  (->> dump
       (map (fn [pixel {x :x}] (if (<= (dec x) (mod pixel 40) (inc x)) "#" ".")) (range))
       (partition 40)
       (mapv (partial clojure.string/join ""))))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day10")))
  ([input]
   (->> input clojure.string/split-lines execute signal-strength)))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day10")))
  ([input]
   (->> input clojure.string/split-lines execute draw-crt)))

(comment
  (solve-1)
  (solve-2))
