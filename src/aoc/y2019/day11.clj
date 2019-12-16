(ns aoc.y2019.day11
  (:require [aoc.y2019.intcode :as intcode]))

(defn move-robot [{:keys [panels robot-position robot-heading :as world-state]} movement]
  (let [[x y] robot-position
        [dx dy new-heading] ({[:north :left] [-1 0 :west] [:north :right] [1 0 :east]
                              [:east :left] [0 1 :north] [:east :right] [0 -1 :south]
                              [:south :left] [1 0 :east] [:south :right] [-1 0 :west]
                              [:west :left] [0 -1 :south] [:west :right] [0 1 :north]}
                             [robot-heading movement])
        new-position [(+ x dx) (+ y dy)]
        panel-color (or (panels new-position) :unpainted)]
    (assoc world-state
           :panels (assoc panels new-position panel-color)
           :robot-position new-position
           :robot-heading new-heading)))

(defn paint-current-panel [{:keys [panels robot-position] :as world-state} color]
  (assoc world-state :panels (assoc panels robot-position color)))

(defn read-current-panel-color [{:keys [panels robot-position]}]
  (if-let [color (panels robot-position)] (if (= :unpainted color) :black color)))

(defn robot-command-issuer [$world-state]
  (let [$robot-command-state (atom :paint)]
    (fn [command]
      (case @$robot-command-state
        :move (do (swap! $world-state move-robot 
                         (case command 0 :left 1 :right))
                  (reset! $robot-command-state :paint))
        :paint (do (swap! $world-state paint-current-panel 
                          (case command 0 :black 1 :white))
                   (reset! $robot-command-state :move))))))

(defn unleash-the-robot [program initial-panel-color]
  (let [$world-state (atom {:panels {[0 0] initial-panel-color}
                            :robot-position [0 0]
                            :robot-heading :north
                            :robot-command-state :move})
        program-input (fn [] (if (= :white (read-current-panel-color @$world-state)) 1 0))
        program-output (robot-command-issuer $world-state)]
    (intcode/compute program program-input program-output)
    @$world-state))

(defn solve-1
  ([] (solve-1 (slurp "input/2019day11")))
  ([input]
   (count (filter #(not= :unpainted (val %)) 
                  (:panels (unleash-the-robot input :black))))))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day11")))
  ([input]
   (let [panels (:panels (unleash-the-robot input :white))
         xs (->> panels keys (map first))
         ys (->> panels keys (map second))
         min-x (- (apply min xs) 5)
         max-x (+ (apply max xs) 5)
         min-y (- (apply min ys) 5)
         max-y (+ (apply max ys) 5)
         image-width (- max-x min-x)]
     (doseq [row (map (partial apply str)
                       (partition image-width
                                  (for [x (range min-x max-x)
                                        y (range min-y max-y)
                                        :let [color (or (get panels [x y]) :black)]]
                                    (if (= color :white) "#" " "))))]
       (println row)))))