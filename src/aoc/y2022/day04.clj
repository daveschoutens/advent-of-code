(ns aoc.y2022.day04
  (:require [aoc.util :refer [parse-int]]))

(defn parse-input [input]
  (->> input
       clojure.string/split-lines
       (map (fn [s]
              (let [[[_ s1 e1 s2 e2] :as k] (re-seq #"^(\d+)-(\d+),(\d+)-(\d+)$" s)]
                [(range (parse-int s1) (inc (parse-int e1)))
                 (range (parse-int s2) (inc (parse-int e2)))])))))

(defn subsumes? [pair]
  (let [[a b] pair
        a (set a)
        b (set b)]
    (or (clojure.set/subset? a b)
        (clojure.set/subset? b a))))

(defn overlap? [pair]
  (let [[a b] pair]
    (not-empty (clojure.set/intersection (set a) (set b)))))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day04")))
  ([input]
   (->> input parse-input (filter subsumes?) count)))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day04")))
  ([input]
   (->> input parse-input (filter overlap?) count)))

(comment
  (solve-1)
  (solve-2))
