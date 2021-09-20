(ns aoc.y2020.day03)

(def sample-input ["..##......."
                   "#...#...#.."
                   ".#....#..#."
                   "..#.#...#.#"
                   ".#...##..#."
                   "..#.##....."
                   ".#.#.#....#"
                   ".#........#"
                   "#.##...#..."
                   "#...##....#"
                   ".#..#...#.#"])

(defn toboggan-map [input]
  (->> input
       (map seq)
       (map cycle)))

(defn solve-1 [input]
  (count (for [row input
               col-ind (iterate #(+ 3 %) 0)
               :let [found ((nth col-ind row))]]
           (if (= found \#) 1 0))))

(comment
  (solve-1 (toboggan-map sample-input))
  (toboggan-map sample-input)
  (take 10 (map (partial take 10) (toboggan-map sample-input)))
  (take 10 (iterate #(+ 3 %) 0))
  )
