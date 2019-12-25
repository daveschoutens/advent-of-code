(ns aoc.y2019.day14
  (:require [clojure.string :as str]
            [aoc.util :refer :all]))

(defn parse-compounds [s]
  (for [cs (str/split s #", ")
        :let [[_ amount compound] (re-matches #"(\d+) (\w+)" cs)]]
    [(keyword compound) (parse-int amount)]))

(defn parse-reaction [s]
  (let [[rs ps] (str/split s #" => ")
        reagents (parse-compounds rs)
        [product-name yield] (first (parse-compounds ps))]
    {:produces product-name :yield yield :ingredients reagents}))

(defn available-reactions [input]
  (group-by :produces (map parse-reaction (str/split-lines input))))

(defn required-raw-materials [rxns thing amount]
  (println "required-raw-materials " thing amount rxns)
  (println "ingredients and stuff " (type (rxns thing)))
  (if-let [{:keys [yield ingredients]} (rxns thing)]
    (reduce (partial merge-with +) {}
            (map (fn [[thing amount]] (required-raw-materials rxns thing amount))
                 ingredients))
    {thing amount}))

(def rxns (available-reactions "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"))

(type rxns)

(required-raw-materials rxns :FUEL 1)