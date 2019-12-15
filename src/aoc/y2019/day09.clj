;; Intcode computer
(defn resolve-param [program relative-base param-mode param]
  (case (Integer/parseInt (str param-mode))
    0 {:mode :position
       :read (or (get program param) 0)
       :write param}
    1  {:mode :immediate
        :read param
        :write param}
    2 {:mode :relative
       :read (or (get program (+ relative-base param)) 0)
       :write (+ relative-base param)}))

(defn interpret-instruction [program pctr relative-base]
  (let [[opcode p1 p2 p3] (drop pctr program)
        [p3-mode p2-mode p1-mode & op-chars] (format "%05d" opcode)]
      {:op (case (Integer/parseInt (apply str op-chars))
             1 :add
             2 :mult
             3 :input
             4 :output
             5 :jump-if-true
             6 :jump-if-false
             7 :less-than
             8 :equals
             9 :relative-base-offset
             99 :halt)
       :p1 (resolve-param program relative-base p1-mode p1)
       :p2 (resolve-param program relative-base p2-mode p2)
       :p3 (resolve-param program relative-base p3-mode p3)}))

(defn write-value [program addr value]
  (let [diff (- addr (count program))
        program (if (> diff 0) (into program (repeat diff 0)) program)]
    (assoc program addr value)))

(defn compute
  ([program] (compute program (fn [])))
  ([program input-fn] (compute program input-fn (fn [_])))
  ([program input-fn output-fn]
   (loop [{:keys [program pctr relative-base output] :as state}
          {:program program :pctr 0 :relative-base 0 :output []}]
     (let [{:keys [op p1 p2 p3] :as instr} (interpret-instruction program pctr relative-base)]
       (case op
         :add
         (recur (assoc state
                       :program (write-value program (:write p3) (+ (:read p1) (:read p2)))
                       :pctr (+ 4 pctr)))
         :mult
         (recur (assoc state
                       :program (write-value program (:write p3) (* (:read p1) (:read p2)))
                       :pctr (+ 4 pctr)))
         :input
         (recur (assoc state
                       :program (write-value program (:write p1) (input-fn))
                       :pctr (+ 2 pctr)))
         :output
         (do (output-fn (:read p1))
             (recur (assoc state
                           :pctr (+ 2 pctr)
                           :output (conj output (:read p1)))))
         :jump-if-true
         (recur (assoc state
                       :program program
                       :pctr (if (not= 0 (:read p1)) (:read p2) (+ 3 pctr))))
         :jump-if-false
         (recur (assoc state
                       :program program
                       :pctr (if (= 0 (:read p1)) (:read p2) (+ 3 pctr))))
         :less-than
         (recur (assoc state
                       :program (write-value program (:write p3) (if (< (:read p1) (:read p2)) 1 0))
                       :pctr (+ 4 pctr)))
         :equals
         (recur (assoc state
                       :program (write-value program (:write p3) (if (= (:read p1) (:read p2)) 1 0))
                       :pctr (+ 4 pctr)))
         :relative-base-offset
         (recur (assoc state
                       :relative-base (+ relative-base (:read p1))
                       :pctr (+ 2 pctr)))
         :halt
         {:program program :status :done :output output :result (last output)})))))
;; End Intcode computer

(defn interactive-input-fn [] (Integer/parseInt (read-line)))

(defn non-interactive-input-factory [inputs]
  (let [input-atom (atom inputs)]
    (fn []
      (let [this-input (first @input-atom)]
        (swap! input-atom rest)
        this-input))))

;; Day 9 program
(def day9-input
  (mapv #(Integer/parseInt %)
        (-> (slurp "input")
            clojure.string/trim
            (clojure.string/split #","))))

(comment
  (compute [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]) ;; itself in output
  (compute [104,1125899906842624,99]) ;; output middle number
  (compute [1102,34915192,34915192,7,4,7,99,0]) ;; 16-digit number
 ) 

;; Day 9 Solution part 1
(compute day9-input (non-interactive-input-factory [1]))
;; Part 2
(compute day9-input (non-interactive-input-factory [2]))