(def initial-input (mapv clojure.edn/read-string (clojure.string/split (slurp "input") #",")))

(def input (assoc initial-input 1 12 2 2))

(defn exec [program]
  (loop [prog program pctr 0]
    (let [[op ptr1 ptr2 dest] (drop pctr prog)
          a (get prog ptr1)
          b (get prog ptr2)
          next-pctr (+ 4 pctr)]
      (case op
        1 (recur (assoc prog dest (+ a b)) next-pctr)
        2 (recur (assoc prog dest (* a b)) next-pctr)
        99 prog))))

(defn run-program [noun verb]
  (let [input (assoc initial-input 1 noun 2 verb)]
    (first (exec input))))

;; Solution 1
(run-program 12 2)

;; Solution 2
(for [noun (range 0 100)
      verb (range 0 100)
      :let [result (run-program noun verb)]
      :when (= 19690720 result)]
  (+ (* 100 noun) verb))