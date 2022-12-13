(ns aoc.y2022.day09)

(defn move-rope [knot-count steps]
  (reductions
   (fn [state step]
     (let [offset (case step :R [1 0] :L [-1 0] :U [0 1] :D [0 -1])
           state (update state 0 #(mapv + % offset))]
       (loop [state state k1-index 0 offset offset]
         (if (<= knot-count (inc k1-index))
           state
           (let [k1 (get state k1-index)
                 k2-index (inc k1-index)
                 k2 (get state k2-index)
                 k2-delta (mapv (comp abs -) k1 k2)
                 k2-offset (if (<= 2 (apply max k2-delta)) (mapv compare k1 k2) [0 0])
                 k2 (mapv + k2 k2-offset)
                 state (assoc state k2-index k2)]
             (recur state k2-index k2-offset))))))
   (into [] (repeat knot-count [0 0]))
   steps))

(defn steps [input]
  (->> input
       clojure.string/split-lines
       (map #(let [[_ direction magnitude] (re-matches #"([RLDU]) (\d+)" %)]
               [(keyword direction) (parse-long magnitude)]))
       (mapcat #(let [[x n] %] (repeat n x)))))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day09")))
  ([input]
   (->> (steps input) (move-rope 2) (map last) set count)))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day09")))
  ([input]
   (->> (steps input) (move-rope 10) (map last) set count)))

(comment
  (solve-1)
  (solve-2))
