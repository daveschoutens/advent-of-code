;; Intcode computer
(defn resolve-param [program param-mode param]
  (let [mode (case (Integer/parseInt (str param-mode))
               0 :position
               1 :immediate)]
    {:mode mode
     :read (case mode :position (get program param) :immediate param)
     :write param}))

(defn interpret-instruction [program pctr]
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
             99 :halt)
       :p1 (resolve-param program p1-mode p1)
       :p2 (resolve-param program p2-mode p2)
       :p3 (resolve-param program p3-mode p3)}))

  (defn compute 
    ([program input-fn] (compute program input-fn (fn [_])))
    ([program input-fn output-fn]
     (loop [{:keys [program pctr output] :as state} {:program program :pctr 0 :output []}]
       (let [{:keys [op p1 p2 p3]} (interpret-instruction program pctr)]
         (case op
           :add
           (recur (assoc state
                         :program (assoc program (:write p3) (+ (:read p1) (:read p2)))
                         :pctr (+ 4 pctr)))
           :mult
           (recur (assoc state
                         :program (assoc program (:write p3) (* (:read p1) (:read p2)))
                         :pctr (+ 4 pctr)))
           :input
           (recur (assoc state
                         :program (assoc program (:write p1) (input-fn))
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
                         :program (assoc program (:write p3) (if (< (:read p1) (:read p2)) 1 0))
                         :pctr (+ 4 pctr)))
           :equals
           (recur (assoc state
                         :program (assoc program (:write p3) (if (= (:read p1) (:read p2)) 1 0))
                         :pctr (+ 4 pctr)))
           :halt
           {:program program :status :done :result (last output)})))))
;; End Intcode computer

(defn interactive-input-fn [] (Integer/parseInt (read-line)))

(defn non-interactive-input-factory [inputs]
  (let [input-atom (atom inputs)]
    (fn []
      (let [this-input (first @input-atom)]
        (swap! input-atom rest)
        this-input))))

(defn pipe [& initial-values]
  (let [buffer (atom (into [] initial-values))]
    {:pull (fn [] 
            (if-let [nextval (first @buffer)] 
              (do (swap! buffer rest)
                  nextval))
            nil)
     :push (fn [out] (swap! buffer conj out))}))

(defn async-pipe [& initial-values]
  (let [q (new java.util.concurrent.LinkedBlockingQueue initial-values)]
    {:out (fn [] (.take q))
     :in (fn [out] (.put q out))}))

;; Day 7 program
(def day7-input
  (mapv #(Integer/parseInt %)
        (-> (slurp "input")
            clojure.string/trim
            (clojure.string/split #","))))

(defn amp-chain-compute [program initial-input phase-settings]
  (let [[a b c d e] phase-settings
        {e-out :in a-in :out} (async-pipe a initial-input)
        {a-out :in b-in :out} (async-pipe b)
        {b-out :in c-in :out} (async-pipe c)
        {c-out :in d-in :out} (async-pipe d)
        {d-out :in e-in :out} (async-pipe e)
        amp (partial compute program)
        amp-a (future (amp a-in a-out))
        amp-b (future (amp b-in b-out))
        amp-c (future (amp c-in c-out))
        amp-d (future (amp d-in d-out))
        amp-e (future (amp e-in e-out))]
    (:result (deref amp-e))))

(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

;; Solution part 1
(->> (permutations (range 0 5))
     (map #(amp-chain-compute day7-input 0 %))
     (apply max))

;; Solution part 2
(->> (permutations (range 5 10))
     (map #(amp-chain-compute day7-input 0 %))
     (apply max))

(comment
  (compute [3,0,4,0,99] interactive-input-fn)
  (compute [3,0,4,0,99] (non-interactive-input-factory [1]))
  (:return (compute day7-input interactive-input-fn)))
