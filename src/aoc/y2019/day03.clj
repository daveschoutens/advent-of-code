(def input (clojure.string/split-lines (slurp "input")))

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

(def wire-paths (map parse-wire-path input))

(def wires 
  (->> wire-paths
       (map wire-vecs)
       (map wire-coords)))

(def intersection-points
  (->> wires
       (map #(into #{} %))
       (apply clojure.set/intersection)
       (remove #(= [0 0] %))))

;; Solution 1
(->> intersection-points
     (sort-by (fn [coords] (reduce + (map #(Math/abs %) coords))))
     (first)
     (map #(Math/abs %))
     (reduce +))

(defn combined-path-to-point [point]
  (->> wires
       (map (partial vec))
       (map #(.indexOf % point))
       (reduce +)))

;; Solution 2
(->> intersection-points
     (map combined-path-to-point)
     sort
     first)