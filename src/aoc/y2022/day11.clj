(ns aoc.y2022.day11
  (:require [clojure.string :as s]))

(defn monkey [s]
  (let [regex (re-pattern
               (s/join "\n.*" ["Monkey (\\d+):"
                               "Starting items: (.*)"
                               "Operation: new = old ([+*]) (.*)"
                               "Test: divisible by (\\d+)"
                               "If true: throw to monkey (\\d+)"
                               "If false: throw to monkey (\\d+)"]))
        [_ id items op operand mod-test if-true if-false :as foo] (re-matches regex s)
        op (case op "+" + "*" * op)
        op-fn (case operand "old" #(op % %) #(op (parse-long operand) %))]
    {:id (parse-long id)
     :items (mapv parse-long (s/split items #", "))
     :op op-fn
     :mod-test (parse-long mod-test)
     :if-true (parse-long if-true)
     :if-false (parse-long if-false)}))

(defn monkeys [input]
  (->> (clojure.string/split (s/trim input) #"\n\n")
       (mapv monkey)))

(defn turn
  [worry-reducer {:keys [items op mod-test if-true if-false] :as monkey}]
  (reduce
   (fn [throws item]
     (let [worry (worry-reducer (op item))
           thrown-to (if (= 0 (mod worry mod-test)) if-true if-false)]
       (update throws thrown-to conj worry)))
   {if-true [] if-false []}
   items))

(defn round [worry-reducer monkeys]
  (reduce
   (fn [monkeys monkey-id]
     (let [monkey (get monkeys monkey-id)
           throws (turn worry-reducer monkey)]
       (-> monkeys
           (assoc-in [monkey-id :items] [])
           (update-in [monkey-id :inspections] (fnil + 0) (count (:items monkey)))
           (update-in [(-> throws first key) :items] into (-> throws first val))
           (update-in [(-> throws second key) :items] into (-> throws second val)))))
   monkeys
   (map :id monkeys)))

(defn solve-it [num-rounds worry-reducer monkeys]
  (let [end-state (nth (iterate (partial round worry-reducer) monkeys) num-rounds)]
    (->> end-state
         (map :inspections)
         (sort >)
         (take 2)
         (reduce *))))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day11")))
  ([input]
   (solve-it 20 #(clojure.math/floor-div % 3) (monkeys input))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day11")))
  ([input]
   (let [monkeys (monkeys input)
         mod-cf (reduce * (map :mod-test monkeys))]
     (solve-it 10000 #(mod % mod-cf) monkeys))))

(comment
  (solve-1)
  (solve-2))
