(ns aoc.y2022.day12
  (:require [clojure.string :as s]))

(defn terrain [input]
  (->> input clojure.string/split-lines
       (mapv (comp vec seq))))

(defn allowed? [start end]
  (let [normalize #(int (case % \S \a \E \z %))]
    (>= (dec (normalize end)) (inc (normalize start)) (normalize end))))

(defn find-pos [terrain token]
(->> terrain
     (map-indexed (fn [i x] (let [j (.indexOf x token)] (when (<= 0 j) [i j]))))
     (filter some?)
     set))

(defn allowed-moves [terrain [x y :as pos]]
(let [current-height (get-in terrain pos)
      all-moves [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]]]
  (filter
   (fn [move]
     (when-let [height (get-in terrain move)]
       (allowed? current-height height)))
   all-moves)))

(defn shortest-path-steps [terrain start end]
  (let [start (first (find-pos terrain start))
        goal (find-pos terrain end)]
    (loop [step 1 visited #{start} frontier [start]]
      (let [new-frontier
            (clojure.set/difference
             (set (mapcat (partial allowed-moves terrain) frontier))
             visited)]
        (cond (empty? new-frontier) :failed
              (some new-frontier goal) step
              :else (recur (inc step) (into visited new-frontier) new-frontier))))))

(defn solve-1
([] (solve-1 (slurp "input/2022day12")))
([input]
 (shortest-path-steps (terrain input) \S \E)
 ))

(defn solve-2
([] (solve-2 (slurp "input/2022day12")))
([input]
 (shortest-path-steps (terrain input) \E \a)
 ))

(comment
(def example "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")
(solve-1)
(solve-2 example))
