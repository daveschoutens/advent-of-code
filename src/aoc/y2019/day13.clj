(ns aoc.y2019.day13
  (:require [aoc.y2019.intcode :as cpu]
            [aoc.util :as util]))

(defn evaluate-triple [[x y tile-id]]
  (if (= [-1 0] [x y])
    {:type :score :points tile-id}
    {:pos [x y] :type (case tile-id 0 :empty 1 :wall 2 :block 3 :paddle 4 :ball)}))

(defn evaluate-output [output]
  (reduce (fn [acc {:keys [pos type]}] 
            (assoc acc pos type)) {} (map evaluate-triple (partition 3 output))))

(defn evaluate-output-for-game [output]
  (group-by :type (map evaluate-triple (partition 3 output))))

(defn solve-1
  ([] (solve-1 (slurp "input/2019day13")))
  ([input]
   (:block (frequencies (vals (evaluate-output (:output (cpu/compute input))))))))

(defn play-game [input]
  (let [is-game-started?$ (atom false)
        output-stream$ (atom [])
        output-collector (fn [out] (swap! output-stream$ conj out))
        player-input (fn []
                       (if (not @is-game-started?$) 1
                           (let [game-state (evaluate-output-for-game @output-stream$)
                                 ball-x (ffirst (:ball game-state))
                                 paddle-x (ffirst (:paddle game-state))]
                             (reset! @output-stream$ [])
                             (compare ball-x paddle-x))))]
    (:score (evaluate-output-for-game (:output (cpu/compute input player-input output-collector))))))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day13")))
  ([input] (play-game input)))