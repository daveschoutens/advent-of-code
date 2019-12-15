(def input (clojure.string/split-lines (slurp "input")))

(defn orbits [input]
  (apply assoc {}
         (->> input
              (map #(clojure.string/split % #"\)"))
              (mapcat reverse)
              (map keyword))))

(defn path-to-com [planet orbits]
  (loop [planet planet path []]
    (if-let [next-in-path (planet orbits)]
      (recur next-in-path (conj path (planet orbits)))
      path)))

;; Solution 1
(let [orbits (orbits input)]
  (->> (keys orbits)
       (map #(path-to-com % orbits))
       (map count)
       (reduce +)))

;; Solution 2
(let [orbits (orbits input)
      you-to-com (set (path-to-com :YOU orbits))
      san-to-com (set (path-to-com :SAN orbits))
      common-path-to-com (clojure.set/intersection you-to-com san-to-com)]
     (+ (count you-to-com)
        (count san-to-com)
        (* -2 (count common-path-to-com))))