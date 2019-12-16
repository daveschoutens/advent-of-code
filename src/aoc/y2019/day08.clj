(ns aoc.y2019.day08)

(defn input->data [input] (clojure.string/trim input))

(defn solve-1
  ([] (solve-1 (slurp "input/2019day08") 25 6))
  ([input width height]
   (let [layers (partition (* width height) (input->data input))
         layer-with-least-0s (apply min-key #(get % \0) (map frequencies layers))
         {ones \1 twos \2} layer-with-least-0s]
     (* ones twos))))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day08") 25 6))
  ([input width height]
   (let [layers (partition (* width height) (input->data input))
         layered-pixels (partition (count layers) (apply interleave layers))
         composed-pixels (map (fn [layered-pixel] (first (filter #(not= \2 %) layered-pixel))) layered-pixels)
         rendered-pixels (map #(case % \1 "#" \0 " ") composed-pixels)]
     (doseq [row (partition width rendered-pixels)]
       (println (apply str row))))))