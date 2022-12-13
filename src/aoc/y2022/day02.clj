(ns aoc.y2022.day02)

(defn rps-decode [c]
  (case c
    \A :rock \B :paper \C :scissors
    \X :rock \Y :paper \Z :scissors))

(defn outcome-decode [c]
  (case c
    \X :lose \Y :draw \Z :win))

(defn choose-shape [them outcome]
  (if (= :draw outcome)
    them
    (case [them outcome]
      [:rock :lose] :scissors
      [:rock :win] :paper
      [:paper :lose] :rock
      [:paper :win] :scissors
      [:scissors :lose] :paper
      [:scissors :win] :rock)))

(defn outcome-score [them us]
  (if (= them us)
    3
    (case [them us]
      [:rock :paper] 6
      [:rock :scissors] 0
      [:paper :rock] 0
      [:paper :scissors] 6
      [:scissors :rock] 6
      [:scissors :paper] 0)))

(defn shape-score [shape]
  (case shape :rock 1 :paper 2 :scissors 3))

(defn score-hand [[them us]]
  (+ (outcome-score them us) (shape-score us)))

(defn parse-input [input]
  (->> input clojure.string/split-lines
       (map (fn [[them _ us]] [(rps-decode them) (rps-decode us)]))))

(defn parse-input-2 [input]
  (->> input clojure.string/split-lines
       (map (fn [[them _ us]]
              (let [them (rps-decode them)
                    us (choose-shape them (outcome-decode us))]
                [them us])))))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day02")))
  ([input]
   (->> input parse-input (map score-hand) (reduce +))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day02")))
  ([input]
   (->> input parse-input-2 (map score-hand) (reduce +))))

(comment
  (solve-1)
  (solve-2))
