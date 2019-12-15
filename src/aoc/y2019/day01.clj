(ns aoc.y2019.day01)

(defn input->data [input] (map #(Integer/parseInt %) (clojure.string/split-lines input)))

(defn fuel-needed [mass]
  (- (Math/floorDiv mass 3) 2))

(defn all-fuel-needed [module-masses]
  (reduce + (map fuel-needed module-masses)))

(defn sum-all [func xs]
  (reduce + (map func xs)))

(defn solve-1
  ([] (solve-1 (slurp "input/2019day01")))
  ([input] (sum-all fuel-needed (input->data input))))

(defn accurate-fuel-needed [mass]
  (let [fuel-for-mass (fuel-needed mass)
        fuel-for-fuel (fuel-needed fuel-for-mass)]
    (cond
      (neg? fuel-for-mass) 0
      (neg? fuel-for-fuel) fuel-for-mass
      :else (+ fuel-for-mass fuel-for-fuel (accurate-fuel-needed fuel-for-fuel)))))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day01")))
  ([input] (sum-all accurate-fuel-needed (input->data input))))