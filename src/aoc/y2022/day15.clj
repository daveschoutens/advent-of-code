(ns aoc.y2022.day15
  (:require [clojure.string :as s]))

(def parse-regex #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(def absdiff (comp abs -))

(defn dist [[ax ay] [bx by]]
  (+ (absdiff ax bx) (absdiff ay by)))

(defn sensors [input]
  (->> (s/split-lines input)
       (map #(let [[_ sx sy bx by] (re-matches parse-regex %)
                   s [(parse-long sx) (parse-long sy)]
                   b [(parse-long bx) (parse-long by)]]
               {:sensor s :beacon b :dist (dist s b)}))))

(defn relevant-sensors [y sensors]
  (filter #(<= (absdiff y (-> % :sensor second)) (:dist %)) sensors))

(defn rangeset [ranges]
  (reduce
   (fn [rs [a2 b2 :as range]]
     (let [[a1 b1 :as last-range] (last rs)]
       (cond (nil? last-range) [range]
             ;; [a1-----b1]
             ;; --[a2-b2]--
             (<= b2 b1) rs ; subsumed
             ;; [a1---b1]---
             ;; ---[a2---b2]
             (<= a2 b1) (conj (butlast rs) [a1 b2]) ; extended
             ;; [a1-b1]-------
             ;; -------[a2-b2]
             (< b1 a2) (conj rs [a2 b2]) ; disjoint
             )))
   []
   (sort ranges)))

(defn coverage [y sensors & [min-bound max-bound]]
  (reduce
   (fn [coverage {[sx sy] :sensor dist :dist}]
     (let [spread (- dist (absdiff sy y))
           cov (if (some? min-bound)
                 [(min min-bound (- sx spread)) (max max-bound (+ sx spread))]
                 [(- sx spread) (+ sx spread)])]
       (rangeset (conj coverage cov))))
   []
   sensors))

(defn solve-1
  ([] (solve-1 2000000 (slurp "input/2022day15")))
  ([y input]
   (let [sensors (sensors input)
         rs (relevant-sensors y sensors)
         known-beacons (->> rs (map :beacon) set (filter #(= y (second %))) count)]
     (->> (coverage y rs)
          (map (fn [[a b]] (inc (- b a))))
          (reduce +)
          (+ (- known-beacons))))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day15")))
  ([input]
   (let [sensors (sensors input)
         rs (relevant-sensors 1 sensors)]
     #_(for [y (range 4000001)])
     (->> (coverage 1 sensors 0 400)
          (map (fn [[a b]] (inc (- b a))))
          (reduce +)))))

(comment
  (solve-1)
  (solve-2)
  (def example "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"))
