(ns aoc.y2019.day06)

(defn input->data [input] (clojure.string/split-lines input))

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

(defn solve-1
  ([] (solve-1 (slurp "input/2019day06")))
  ([input]
   (let [orbits (orbits (input->data input))]
     (->> (keys orbits)
          (map #(path-to-com % orbits))
          (map count)
          (reduce +)))))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day06")))
  ([input]
   (let [orbits (orbits (input->data input))
         you-to-com (set (path-to-com :YOU orbits))
         san-to-com (set (path-to-com :SAN orbits))
         common-path-to-com (clojure.set/intersection you-to-com san-to-com)]
     (+ (count you-to-com)
        (count san-to-com)
        (* -2 (count common-path-to-com))))))