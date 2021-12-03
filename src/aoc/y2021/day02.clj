(ns aoc.y2021.day02
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "input/2021day02")))

(defn dir-vector [input-line]
  (let [[direction-str magnitude-str] (str/split input-line #" ")
        magnitude (Integer/parseInt magnitude-str)]
    (case direction-str
      "forward" [magnitude 0]
      "up" [0 (- magnitude)]
      "down" [0 magnitude]
      :else nil)))

(defn solve-1 [input]
  (apply * (apply mapv (conj (map dir-vector input) +))))

(defn move [{:keys [x y aim] :as state} input-line]
  (let [[direction-str magnitude-str] (str/split input-line #" ")
        magnitude (Integer/parseInt magnitude-str)]
    (case direction-str
      "forward" {:x (+ x magnitude) :y (+ y (* aim magnitude)) :aim aim}
      "up"      {:x x :y y :aim (- aim magnitude)}
      "down"    {:x x :y y :aim (+ aim magnitude)}
      :else     state)))

(defn solve-2 [input]
  (let [{:keys [x y aim] :as final-state} (reduce move {:x 0 :y 0 :aim 0} input)]
    (* x y)))

(comment
  (solve-1 input)
  (solve-2 input))
