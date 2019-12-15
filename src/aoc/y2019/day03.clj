(ns aoc.y2019.day03)

(defn input->data [input] (clojure.string/split-lines input))

(defn parse-wire-path [wire-path]
  (map (fn [step-str]
         {:direction (case (first step-str) \U :up \D :down \L :left \R :right)
          :distance (Integer/parseInt (apply str (rest step-str)))})
       (clojure.string/split wire-path #",")))

(defn dir-vec [direction] 
  (case direction
    :up [0 1] 
    :down [0 -1] 
    :left [-1 0] 
    :right [1 0]))

(defn step-vecs [wire-path-step]
  (let [{:keys [direction distance]} wire-path-step]
    (repeat distance (dir-vec direction))))

(defn wire-vecs [wire-path]
  (mapcat step-vecs wire-path))

(defn wire-coords [wire-vecs]
  (reductions 
   (fn [[last-x last-y] [delta-x delta-y]] [(+ last-x delta-x) (+ last-y delta-y)])
   [0 0]
   wire-vecs))

(defn wires [input]
  (->> (map parse-wire-path input)
       (map wire-vecs)
       (map wire-coords)))

(defn intersection-points [wires]
  (->> wires
       (map #(into #{} %))
       (apply clojure.set/intersection)
       (remove #(= [0 0] %))))

(defn solve-1
  ([] (solve-1 (slurp "input/2019day03")))
  ([input]
   (->> (input->data input)
        wires
        intersection-points
        (sort-by (fn [coords] (reduce + (map #(Math/abs %) coords))))
        (first)
        (map #(Math/abs %))
        (reduce +))))

(defn combined-path-to-point [wires point]
  (->> wires
       (map (partial vec))
       (map #(.indexOf % point))
       (reduce +)))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day03")))
  ([input]
   (let [wires (wires (input->data input))]
     (->> (intersection-points wires)
          (map (partial combined-path-to-point wires))
          sort
          first))))