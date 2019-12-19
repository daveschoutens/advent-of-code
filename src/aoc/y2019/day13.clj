(ns aoc.y2019.day13
  (:require [aoc.y2019.intcode :as cpu]
            [aoc.util :as util]))

(defn is-score? [[x y _]]
  (= [-1 0] [x y]))

(defn evaluate-triple [[x y tile-id :as triple]]
  (if (is-score? triple)
    {:pos [x y] :type :score :points tile-id}
    {:pos [x y] :type (case tile-id 0 :empty 1 :wall 2 :block 3 :paddle 4 :ball)}))

(defn evaluate-output [output]
  (reduce (fn [acc {:keys [pos type]}] 
            (assoc acc pos type)) {} (map evaluate-triple (partition 3 output))))

(defn evaluate-output-for-game [output]
  (group-by :type (map evaluate-triple (partition 3 output))))

(defn solve-1   
  ([] (solve-1 (slurp "input/2019day13")))
  ([input]
   (:block (frequencies (vals (evaluate-output (:output (cpu/compute (cpu/parse-program input)))))))))

(defn play-game [input]
  (let [playable-game (assoc (cpu/parse-program input) 0 2)
        output-stream$ (atom [])
        output-collector (fn [out] (swap! output-stream$ conj out))
        player-input (fn []
                       (let [game-state (evaluate-output-for-game @output-stream$)
                             ball-x (get (second (ffirst (:ball game-state))) 0)
                             paddle-x (get (second (ffirst (:paddle game-state))) 0)
                             move (compare ball-x paddle-x)]
                         (reset! output-stream$ [])
                         move))]
    (:result (cpu/compute playable-game player-input output-collector))))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day13")))
  ([input] (play-game input)))