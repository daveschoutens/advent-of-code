(ns aoc.y2019.day15
  (:require [aoc.y2019.intcode :as cpu]
            [clojure.tools.trace :as trace]))

;; 1 north 2 south 3 west 4 east
;; 0 wall 1 moved 2 moved & found sensor

(defn next-move [maze pos]
  (let [not-explored? nil? ;; readability alias
        step-north (mapv + pos [0 1])
        step-south (mapv + pos [0 -1])
        step-east (mapv + pos [1 0])
        step-west (mapv + pos [-1 0])]
    (cond
      (not-explored? (maze step-north)) step-north
      (not-explored? (maze step-south)) step-south
      (not-explored? (maze step-west)) step-west
      (not-explored? (maze step-east)) step-east
      :else nil)))

(defn delta-a->b [a b]
  (tap> a)
  (tap> b)
  (mapv - b a))

(defn encode-direction-instruction [delta]
  ({[0 1]  1 ;north
    [0 -1] 2 ;south
    [1 0]  3 ;west
    [-1 0] 4 ;east
    } delta))

(defn depth-first-map-reveal [program]
  (let [breadcrumbs$ (atom '([0 0]))
        maze$ (atom {})
        output-fn (fn [out]
                    (let [pos (first @breadcrumbs$)
                          found (case out 0 :wall 1 :path 2 :sensor)]
                      (swap! maze$ assoc pos found)
                      (when (= found :wall) (swap! breadcrumbs$ pop))))
        input-fn (fn []
                   (let [pos (first @breadcrumbs$)]
                     (if-let [next-pos (next-move @maze$ pos)]
                       (do (swap! breadcrumbs$ conj next-pos)
                           (encode-direction-instruction (delta-a->b pos next-pos)))
                       (if-let [backtrack (fnext @breadcrumbs$)]
                         (do (swap! breadcrumbs$ pop)
                             (encode-direction-instruction (delta-a->b pos backtrack)))
                         nil #_abort-program))))
        _ (cpu/compute (cpu/parse-program program) input-fn output-fn)]
    @maze$))

(defn breadth-first-search [maze start-pos goal]
  (loop [paths [(list start-pos)] visited #{start-pos} loopcount 1]
    (let [visited-this-round$ (atom #{})
          dir-vecs [[0 1] [0 -1] [1 0] [-1 0]]
          new-paths (for [path paths
                          dir dir-vecs
                          :let [pos (first path)
                                next-step (mapv + dir pos)]
                          :when (and (not= :wall (maze next-step))
                                     (not (visited next-step))
                                     (not (@visited-this-round$ next-step)))]
                      (do (swap! visited-this-round$ conj next-step)
                          (conj path next-step)))]
      (if (>= loopcount 500)
        {:status :quit-early :path-length loopcount}
        (if (empty? new-paths)
          {:status :exhausted-all-paths :max-path-length (dec loopcount)}
          (if-let [goal-finding-path (first (filter #(= goal (maze (first %))) new-paths))]
            {:status :found-goal :path-length loopcount :real-path-length (dec (count goal-finding-path)) :path goal-finding-path}
            (recur new-paths (clojure.set/union visited @visited-this-round$) (inc loopcount))))))))

(defn draw-maze [maze]
  (let [xs (->> maze keys (map first))
        ys (->> maze keys (map second))
        min-x (- (apply min xs) 5)
        max-x (+ (apply max xs) 5)
        min-y (- (apply min ys) 5)
        max-y (+ (apply max ys) 5)
        image-width (- max-x min-x)]
       (doseq [row (map (partial apply str)
                        (partition image-width
                                   (for [y (reverse (range min-y max-y))
                                         x (range min-x max-x)
                                         :let [wut (or (get maze [x y]) :path)]]
                                     (cond
                                       (= 0 x y) "@"
                                       (= :wall wut) "#"
                                       (= :sensor wut) "&"
                                       :else " "))))]
         (println row))))

(defn solve-1 []
  (let [maze (depth-first-map-reveal (slurp "input/2019day15"))]
    (breadth-first-search maze [0 0] :sensor)))

(defn solve-2 []
  (let [maze (depth-first-map-reveal (slurp "input/2019day15"))]
    (breadth-first-search maze [-12 -12] :nothing)))