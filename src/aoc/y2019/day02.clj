(ns aoc.y2019.day02)

(defn input->data [input] 
  (mapv #(Integer/parseInt %) (clojure.string/split (clojure.string/trim input) #",")))

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

(defn run-program [program noun verb]
  (let [input (assoc program 1 noun 2 verb)]
    (first (exec input))))

(defn solve-1
  ([] (solve-1 (slurp "input/2019day02")))
  ([input] (run-program (input->data input) 12 2)))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day02")))
  ([input]
   (first (for [noun (range 0 100)
                verb (range 0 100)
                :let [result (run-program (input->data input) noun verb)]
                :when (= 19690720 result)]
            (+ (* 100 noun) verb)))))