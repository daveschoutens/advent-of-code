(ns aoc.y2022.day09)

(defn move-rope [steps]
  (reductions
   (fn [{:keys [h t t-path] :as state} direction]
     (let [h-offset (case direction :R [1 0] :L [-1 0] :U [0 1] :D [0 -1])
           h (mapv + h h-offset)
           t-offset (mapv (comp #(/ 2.0) -) h t) #_(mapv compare h t)
           t (mapv + t t-offset)]
       (-> state
           (assoc :h h)
           (assoc :t t)
           (update :t-path conj t))
       ))
   {:h [0 0]
    :t [0 0]
    :t-path [[0 0]]}
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
   (->> (steps input) move-rope)))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day09")))
  ([input]))

(comment
  (def example "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")
  (solve-1 example)
  (solve-2))
