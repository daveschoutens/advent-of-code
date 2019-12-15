(ns aoc.y2018.day01)

(defn input->data [input] 
  (map #(Integer/parseInt %) (clojure.string/split input #"\n")))

(defn infinite-rolling-sum [ints]
  (reductions + (cycle ints)))

(defn find-repeated [s]
  (loop [s s
         xs #{}]
    (let [x (first s)]
      (if (contains? xs x)
        x
        (recur (rest s) (conj xs x))))))

(defn solve-1
  ([] (solve-1 (slurp "input/2018day01")))
  ([input] (reduce + (input->data input))))

(defn solve-2 
  ([] (solve-2 (slurp "input/2018day01")))
  ([input] (find-repeated (infinite-rolling-sum (input->data input)))))