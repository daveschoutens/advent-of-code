(ns aoc.y2022.day06
  (:require [aoc.util :refer [parse-int]]))

(defn markers [stream marker-length]
  (->> stream
       (partition marker-length 1)
       (map-indexed #(do [%1 %2 (= marker-length (count (set %2)))]))
       (filter #(nth % 2))
       (map #(+ marker-length (first %)))))

(defn packet-markers [stream] (markers stream 4))
(defn message-markers [stream] (markers stream 14))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day06")))
  ([input] (first (packet-markers input))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day06")))
  ([input] (first (message-markers input))))

(comment
  (def example "bvwbjplbgvbhsrlpgdmjqwftvncz")
  (solve-1)
  (solve-2))
