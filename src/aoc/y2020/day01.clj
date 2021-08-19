(ns aoc.y2020.day01
  (:require [clojure.string :as str]))

(defn read-input []
  (map #(Integer/parseInt %) (str/split-lines (slurp "input/2020day01"))))

(defn solve-1 [input]
  (first
   (for [x input y input :when (= 2020 (+ x y))]
     (* x y))))

(defn solve-2 [input]
  (first
   (for [x input y input z input :when (= 2020 (+ x y z))]
     (* x y z))))

(comment
  (solve-1 (read-input))
  (solve-2 (read-input)))
