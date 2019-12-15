(def input (map #(Integer/parseInt %) (clojure.string/split (slurp "input") #"\n")))

(defn fuel-needed [mass]
  (- (Math/floorDiv mass 3) 2))

(defn all-fuel-needed [module-masses]
  (reduce + (map fuel-needed module-masses)))

(defn sum-all [func xs]
  (reduce + (map func xs)))

;; Solution 1
(sum-all fuel-needed input)

(defn accurate-fuel-needed [mass]
  (let [fuel-for-mass (fuel-needed mass)
        fuel-for-fuel (fuel-needed fuel-for-mass)]
    (cond
      (neg? fuel-for-mass) 0
      (neg? fuel-for-fuel) fuel-for-mass
      :else (+ fuel-for-mass fuel-for-fuel (accurate-fuel-needed fuel-for-fuel)))))

;; Solution 2
(sum-all accurate-fuel-needed input)

;; Supporting tests

(defn assert-equals [expected actual]
  (assert (= expected actual) (str "Expected " expected " but got " actual)))

(assert-equals 2 (accurate-fuel-needed 14))
(assert-equals 966 (accurate-fuel-needed 1969))
(assert-equals 50346 (accurate-fuel-needed 100756))