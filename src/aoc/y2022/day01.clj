(ns aoc.y2022.day01
  (:require [aoc.util :refer [parse-int]]))

(defn calories-per-elf [input]
  (->> input (partition-by empty?)
       (map (partial filter not-empty))
       (filter not-empty)
       (map (partial map parse-int))
       (map (partial reduce +))))

(defn solve-1
  ([] (solve-1 (clojure.string/split-lines (slurp "input/2022day01"))))
  ([input]
   (->> input
        calories-per-elf
        (reduce max))))

(defn solve-2
  ([] (solve-2 (clojure.string/split-lines (slurp "input/2022day01"))))
  ([input]
   (->> input
        calories-per-elf
        sort
        (take-last 3)
        (reduce +))))

(comment
  (solve-1)
  (solve-2))
