(ns aoc.y2021.day01
  (:require [clojure.string :as str]))

(def sample-input [199
                   200
                   208
                   210
                   200
                   207
                   240
                   269
                   260
                   263])

(def input (map #(Long/parseLong %) (str/split-lines (slurp "input/2021day01"))))

(defn count-increasing [xs]
  (->> (partition 2 1 xs)
       (filter #(apply < %))
       count))

(defn count-increasing-3-sums [xs]
  (->> (partition 3 1 xs)
       (map #(apply + %))
       count-increasing))

(defn solve-1 [input]
  (count-increasing input))

(defn solve-2 [input]
  (count-increasing-3-sums input))

(comment
  (solve-1 input)
  (solve-2 input))
