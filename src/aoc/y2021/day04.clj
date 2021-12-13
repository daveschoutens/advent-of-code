(ns aoc.y2021.day04
  (:require [clojure.string :as str]
            [clojure.set :refer [difference]]))

(defn win-conditions [puzzle]
  (map (partial mapv (vec puzzle))
       [[0 1 2 3 4] [5 6 7 8 9] [10 11 12 13 14] [15 16 17 18 19] [20 21 22 23 24] ;rows
        [0 5 10 15 20] [1 6 11 16 21] [2 7 12 17 22] [3 8 13 18 23] [4 9 14 19 24] ;cols
        ]))

(defn won? [puzzle numbers]
  (let [numbers (set numbers)]
    (seq (->> (win-conditions puzzle)
              (map set)
              (filter #(empty? (difference % numbers)))))))

(defn solve-1 []
  (let [chunks (str/split (slurp "input/2021day04") #"\n\n")
        numbers (map #(Integer/parseInt %) (str/split (first chunks) #", ?"))
        puzzles (->> (rest chunks)
                     (map (fn [x] (-> x
                                      str/trim
                                      (str/split #"\n *")
                                      (->> (mapcat #(str/split % #" +"))
                                           (map #(Integer/parseInt %)))))))]
    (first (for [turn (range 1 (inc (count numbers)))
                 puzzle puzzles
                 :let [nums (take turn numbers)
                       current (last nums)]
                 :when (won? puzzle nums)]
             (let [unmarked (difference (set puzzle) (set nums))
                   sum-unmarked (reduce + unmarked)]
               (clojure.pprint/pprint {:unmarked unmarked :sum sum-unmarked :current current :nums nums})
               (* current sum-unmarked))))))

(solve-1)
