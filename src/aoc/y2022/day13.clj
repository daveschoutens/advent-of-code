(ns aoc.y2022.day13
  (:require [clojure.string :as s]))

(defn parse-input [input]
  (->> (clojure.string/split input #"\n\n")
       (map clojure.string/split-lines)
       (map #(map clojure.edn/read-string %))))

(defn comparev [a b]
  (cond (and (some? a) (nil? b)) 1
        (and (nil? a) (some? b)) -1
        (and (nil? a) (nil? b)) 0
        (and (int? a) (int? b)) (compare a b)
        (and (int? a) (vector? b)) (recur [a] b)
        (and (vector? a) (int? b)) (recur a [b])
        :else ; vector/vector
        (loop [[x :as a] a [y :as b] b]
          (let [result (comparev x y)]
            (if (and (not= [nil nil] [x y]) (= 0 result))
              (recur (rest a) (rest b))
              result)))))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day13")))
  ([input]
   (->> (parse-input input)
        (map-indexed (fn [i [a b]] (if (>= 0 (comparev a b)) (inc i) 0)))
        (reduce +))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day13")))
  ([input]
   (->> (conj (mapcat identity (parse-input input)) [[2]] [[6]])
        (sort comparev)
        (map vector (drop 1 (range)))
        (filter #(#{[[2]] [[6]]} (second %)))
        (map first)
        (reduce *))))

(comment
  (solve-1)
  (solve-2))
