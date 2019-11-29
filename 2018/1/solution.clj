(defn one []
  (reduce + (map #(Integer/parseInt %) 
                 (clojure.string/split (slurp "input") #"\n"))))

(defn infinite-rolling-sum []
  (reductions + (cycle (map #(Integer/parseInt %) 
                            (clojure.string/split (slurp "input") #"\n")))))

(defn find-repeated [s]
  (loop [s s
         xs #{}]
    (let [x (first s)]
      (if (contains? xs x)
        x
        (recur (rest s) (conj xs x))))))

(defn two []
  (find-repeated (infinite-rolling-sum)))