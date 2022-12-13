(ns aoc.y2022.day05
  (:require [aoc.util :refer [parse-int]]))

(defn parse-stacks
  [s]
  (->> (clojure.string/split-lines s)
       (map #(map second (re-seq #"[ \[](.)[ \]] ?" %)))
       reverse
       (#(apply interleave (partition (count (first %)) (flatten %))))
       (partition-by #(re-find #"\d+" %))
       (drop 1)
       (take-nth 2)
       (mapv #(remove clojure.string/blank? %))))

(defn parse-input
  [input]
  (let [[s m] (clojure.string/split input #"\n\n")]
    {:stacks (parse-stacks s)
     :moves (clojure.string/split-lines m)}))

(defn parse-move
  "Returns [num from to]"
  [move]
  (->> (re-matches #"move (\d+) from (\d) to (\d)" move)
       (drop 1)
       (map parse-int)))

(defn operate-crane
  [crane-model {:keys [stacks moves]}]
  (let [tweak (case crane-model :9000 reverse :9001 identity)]
    (reduce
     (fn [stacks move]
       (let [[num from to] (parse-move move)
             crates (-> stacks (nth (dec from)) (#(take-last num %)) tweak)]
         (-> stacks
             (update (dec from) (partial drop-last num))
             (update (dec to) concat crates))))
     stacks
     moves)))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day05")))
  ([input]
   (->> input parse-input (operate-crane :9000) (map last) (apply str))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day05")))
  ([input]
   (->> input parse-input (operate-crane :9001) (map last) (apply str))))

(comment
  (solve-1)
  (solve-2))
