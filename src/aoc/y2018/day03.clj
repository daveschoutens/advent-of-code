(ns aoc.y2018.day03)

(defn input->data [input] (clojure.string/split-lines input))

(defn parse-claim [claim]
  (let [raw-claim-parts (re-matches #"\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)
        [id x y w h] (->> raw-claim-parts (drop 1) (map #(Integer/parseInt %)))]
    {:id id :x x :y y :w w :h h}))

(defn claimed-inches [{:keys [x y w h]}]
  (for [X (range x (+ x w))
        Y (range y (+ y h))]
    [X Y]))

(defn count-overlap-inches [claims]
  (->> claims 
       (mapcat claimed-inches) 
       frequencies 
       (filter #(< 1 (val %))) 
       count))

(defn solve-1
  ([] (solve-1 (slurp "input/2018day03")))
  ([input] (count-overlap-inches (map parse-claim (input->data input)))))

(defn overlaps? [claims-by-coordinate claim]
  (->> (claimed-inches claim)
       (map claims-by-coordinate)
       (filter #(< 1 %))
       ((comp not empty?))))

(defn find-nonoverlapping-claim [claims]
  (let [claims-by-coordinate (frequencies (mapcat claimed-inches claims))]
    (first (remove (partial overlaps? claims-by-coordinate) claims))))

(defn solve-2
  ([] (solve-2 (slurp "input/2018day03")))
  ([input] (:id (find-nonoverlapping-claim (map parse-claim (input->data input))))))