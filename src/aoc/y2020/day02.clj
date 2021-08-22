(ns aoc.y2020.day02
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def input (str/split-lines (slurp "input/2020day02")))

(defn parse-inputline [inputline]
  (prn inputline)
  (let [re-input #"(\d+)-(\d+) ([a-zA-Z]): (.*)"
        [[_ a b letter password]] (re-seq re-input inputline)]
    {:policy [(util/parse-int a) (util/parse-int b) (first letter)]
     :password password}))

(defn eval-password-policy1 [{[least most letter] :policy password :password}]
  (let [letter-freq (get (frequencies password) letter 0)]
    (<= least letter-freq most)))

(defn eval-password-policy2 [{[pos1 pos2 letter] :policy password :password}]
  (let [in-pos? (fn [pos] (= letter (nth password (dec pos))))
        in-pos1? (in-pos? pos1)
        in-pos2? (in-pos? pos2)]
    (or (and in-pos1? (not in-pos2?))
        (and in-pos2? (not in-pos1?)))))

(defn solve [policy-eval-fn input]
  (->> input
       (map parse-inputline)
       #_(filter policy-eval-fn)
       count))

(defn solve-1 [input]
  (solve eval-password-policy1 input))

(defn solve-2 [input]
  (solve eval-password-policy1 input))

(comment
  (solve-1 input)
  (solve-2 input))
