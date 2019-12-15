(def input (clojure.string/split-lines (slurp "input")))

(defn asteroids-from-input [input]
  (for [y (range 0 (count input))
        x (range 0 (count (first input)))
        :let [value (get-in input [y x])]
        :when (= \# value)]
    [x y]))

(defn p->q [[x1 y1] [x2 y2]] [(- x2 x1) (- y2 y1)])

(defn magnitude [[x y]]
  (.sqrt (bigdec (+ (* x x) (* y y))) java.math.MathContext/DECIMAL128))

(defn unit-vector [[x y :as v]]
  (let [mag (magnitude v)]
    [(with-precision 32 (/ x mag)) (with-precision 32 (/ y mag))]))

(defn dot [v1 v2]
  (reduce + (map (partial apply *) (partition 2 (interleave v1 v2)))))

(defn cross2d [[x1 y1] [x2 y2]]
  (- (* x1 y2) (* x2 y1)))

(defn vec-angle [v1 v2]
  (let [theta (Math/acos (dot (unit-vector v1) (unit-vector v2)))
        z (cross2d v1 v2)]
    (if (neg? z) (- (* 2 Math/PI) theta) theta)))

(defn visible-asteroid-count [asteroids position]
  (->> asteroids 
       (filter (partial not= position))
       (map (partial p->q position)) 
       (group-by (partial vec-angle [0 1]))
       frequencies
       keys
       count))

(defn solution1 [input-str]
  (let [input (clojure.string/split-lines input-str)
        asteroids (asteroids-from-input input)]
    (apply max (map (partial visible-asteroid-count asteroids) asteroids))))

(defn solution-position [input-str]
  (let [input (clojure.string/split-lines input-str)
        asteroids (asteroids-from-input input)]
    (apply max-key (partial visible-asteroid-count asteroids) asteroids)))

(defn solution2 [input-str]
  (let [input (clojure.string/split-lines input-str)
        asteroids (asteroids-from-input input)
        best-position (apply max-key (partial visible-asteroid-count asteroids) asteroids)
        sorted-groups (->> asteroids
                           (filter (partial not= best-position))
                           (map (fn [asteroid]
                                  (let [vec (p->q best-position asteroid)
                                        up [0 -1]]
                                    {:asteroid asteroid
                                     :vec vec
                                     :distance (magnitude vec)
                                     :angle (vec-angle up vec)})))
                           (sort-by :distance)
                           (group-by :angle)
                           (into (sorted-map))
                           vals)
        sorted-asteroids (loop [lase-order [] remaining-groups sorted-groups]
                           (if (empty? remaining-groups)
                             lase-order
                             (recur (into lase-order (filter some? (map first remaining-groups)))
                                    (filter not-empty (map rest remaining-groups)))))]
    (nth sorted-asteroids 199)))

;; Solution 1
(solution1 (slurp "input"))

;; Solution 2
(solution2 (slurp "input"))
