(def initial-input (mapv clojure.edn/read-string (clojure.string/split (slurp "input") #",")))

(defn interpret-instruction [program opcode & params]
  (let [[param-3-mode param-2-mode param-1-mode & op-chars :as opv] (format "%05d" opcode)
        resolve-param (fn [param-mode param]
                        (let [mode (case (Integer/parseInt (str param-mode)) 
                                     0 :position 
                                     1 :immediate)]
                          {:mode mode
                           :read-value (case mode 
                                         :position (get program param) 
                                         :immediate param)
                           :write-address param}))]
    {:op (case (Integer/parseInt (apply str op-chars))
           1 :add
           2 :mult
           3 :input
           4 :output
           5 :jump-if-true
           6 :jump-if-false
           7 :less-than
           8 :equals
           99 :halt)
     :p1 (resolve-param param-1-mode (nth params 0))
     :p2 (resolve-param param-2-mode (nth params 1))
     :p3 (resolve-param param-3-mode (nth params 2))}))

(defn exec [program]
  (loop [prog program pctr 0 output []]
    (let [[op p1 p2 p3] (drop pctr prog)
          {:keys [op p1 p2 p3]} (interpret-instruction program op p1 p2 p3)]
      (case (instruction :op)
        :add (recur (assoc prog p3 (+ a b)) (+ 4 pctr) output)
        :mult (recur (assoc prog p3 (* a b)) (+ 4 pctr) output)
        :input (recur (assoc prog p1 (Integer/parseInt (read-line))) (+ 2 pctr) output)
        :output (recur prog (+ 2 pctr) (conj output (get prog p1)))
        :halt {:program prog
               :output output
               :return (last output)}))))

(exec [3,0,4,0,99])