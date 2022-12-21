(ns aoc.y2022.day14
  (:require [clojure.string :as s]))

(defn rock [input]
  (let [paths (->> input s/split-lines (map #(s/split % #" -> ")))]
    (set (for [path paths
               segment (partition 2 1 path)
               :let [[[x1 y1] [x2 y2]]
                     (->> segment (map #(map parse-long (s/split % #","))))
                     line-fn (fn [a b] (if (= a b)
                                         (repeat a)
                                         (apply #(range %1 (inc %2)) (sort [a b]))))]
               point (map vector (line-fn x1 x2) (line-fn y1 y2))]
           point))))

(defn simulate-sand-abyss [rocks sand-source]
  (let [floor-level (apply max (map second rocks))]
    (loop [settled-sand #{} [x y :as pos] sand-source i 0]
      (let [possibles (map (fn [x] [x (inc y)]) [x (dec x) (inc x)])
            available (complement (clojure.set/union rocks settled-sand))
            new-pos (first (filter available possibles))]
        (cond (nil? new-pos) (recur (conj settled-sand pos) sand-source (inc i))
              (< floor-level (second new-pos)) settled-sand
              :else (recur settled-sand new-pos (inc i)))))))

(defn simulate-sand-fill [rocks sand-source]
  (let [floor-level (+ 2 (apply max (map second rocks)))]
    (loop [settled-sand #{} [[x y :as pos] :as path] (list sand-source) i 0]
      (if (nil? pos)
        settled-sand
        (let [possibles (map (fn [x] [x (inc y)]) [x (dec x) (inc x)])
              available (complement (clojure.set/union rocks settled-sand))
              new-pos (first (filter #(and (available %) (> floor-level (second %))) possibles))]
          (if (nil? new-pos)
            (recur (conj settled-sand pos) (rest path) (inc i))
            (recur settled-sand (conj path new-pos) (inc i))))))))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day14")))
  ([input]
   (count (simulate-sand-abyss (rock input) [500 0]))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day14")))
  ([input]
   (count (simulate-sand-fill (rock input) [500 0]))))

(comment
  (solve-1)
  (solve-2))
