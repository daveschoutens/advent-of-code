(def input (line-seq (clojure.java.io/reader "input")))

(defn parse-claim [claim]
  (let [raw-claim-parts (re-matches #"\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)
        [id x y w h] (->> raw-claim-parts (drop 1) (map #(Integer/parseInt %)))]
    {:id id :x x :y y :w w :h h}))

(def input-claims (map parse-claim input))

(defn claimed-inches [{:keys [x y w h]}]
  (for [X (range x (+ x w))
        Y (range y (+ y h))]
    [X Y]))

(defn count-overlap-inches [claims]
  (->> claims 
       (mapcat claimed-inches) 
       frequencies 
       (filter #(< 1 (val %))) 
       count))
  
;; Solution 1
(count-overlap-inches input-claims)

(defn overlaps? [claims-by-coordinate claim]
  (->> (claimed-inches claim)
       (map claims-by-coordinate)
       (filter #(< 1 %))
       ((comp not empty?))))

(defn find-nonoverlapping-claim [claims]
  (let [claims-by-coordinate (frequencies (mapcat claimed-inches claims))]
    (remove (partial overlaps? claims-by-coordinate) claims)))

;; Solution 2
(find-nonoverlapping-claim input-claims)