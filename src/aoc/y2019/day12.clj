(ns aoc.y2019.day12
  (:require [aoc.util :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :as test]))

;; <x=-1, y=0, z=2>
;; <x=2, y=-10, z=-7>
;; <x=4, y=-8, z=8>
;; <x=3, y=5, z=-1>

(defn parse-moon [s]
  (let [[_ x y z] (re-matches #"<x=(.*), y=(.*), z=(.*)>" s)]
    {:pos (mapv parse-int [x y z]) 
     :vel [0 0 0]}))

(defn parse-moons [s]
  (map parse-moon (str/split-lines s)))

(defn single-gravity [moon other-moon]
  (let [{[x1 y1 z1] :pos} moon
        {[x2 y2 z2] :pos} other-moon]
    [(compare x2 x1) (compare y2 y1) (compare z2 z1)]))

(defn gravity [moon other-moons]
  (apply mapv + (map (partial single-gravity moon) other-moons)))

(defn apply-gravity [{:keys [pos vel] :as moon} gravity]
  (let [new-velocity (mapv + vel gravity)]
    (assoc moon 
           :pos (mapv + pos new-velocity)
           :vel new-velocity)))

(defn simulate-step [moons]
  (map #(apply-gravity % (gravity % moons)) moons))

(defn simulate [moons steps]
  (nth (iterate simulate-step moons) steps))

(defn potential-energy [moon]
  (apply + (map #(Math/abs %) (:pos moon))))

(defn kinetic-energy [moon]
  (apply + (map #(Math/abs %) (:vel moon))))

(defn total-energy [moon]
  (* (potential-energy moon) (kinetic-energy moon)))

(defn total-system-energy [moons]
  (apply + (map total-energy moons)))

(defn solve-1
  ([] (solve-1 (slurp "input/2019day12") 1000))
  ([input steps]
   (let [moons (parse-moons input)]
     (total-system-energy (simulate moons steps)))))

(defn moon-axis-period [moons which-moon which-axis]
  (let [{:keys [pos vel] :as moon} (nth moons which-moon)
        which (case which-axis :x 0 :y 1 :z 2)
        initial-pos (nth pos which)
        initial-vel (nth vel which)
        not-initial-state? (fn [moons]
                             (let [{:keys [pos vel]} (nth moons which-moon)]
                               (not (and (= initial-pos (nth pos which))
                                         (= initial-vel (nth vel which))))))]
    (count (take-while not-initial-state? (rest (iterate simulate-step moons))))))

(defn axis-orbital-period [moons axis]
  (let [which (case axis :x 0 :y 1 :z 2)
        get-axis-state (fn [moons] (map #(vector (nth (:pos %1) which) (nth (:vel %1) which)) moons))
        initial-state (get-axis-state moons)
        initial-state? (fn [moons] (= initial-state (get-axis-state moons)))]
    (inc (count (take-while (comp not initial-state?) (rest (iterate simulate-step moons)))))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* (bigint a) (bigint b)) (gcd a b)))

(defn lcmv [& v] 
  (reduce lcm v))

(defn solve-2
  ([] (solve-2 (slurp "input/2019day12")))
  ([input]
   (let [moons (parse-moons input)]
     (apply lcmv (map (partial axis-orbital-period moons) [:x :y :z])))))

(and
 (test/is (= {:pos [-1 0 2] :vel [0 0 0]} (parse-moon "<x=-1, y=0, z=2>")))
 (test/is (= [-1 0 1] (single-gravity {:pos [0 0 0]} {:pos [-2 0 2]})))
 (test/is (= [-1 0 1] (gravity {:pos [0 0 0]} [{:pos [-2 0 2]}])))
 (test/is (= [-1 0 3] (gravity {:pos [0 0 0]} [{:pos [-2 0 2]} {:pos [1 2 3]} {:pos [-5 -2 7]}])))
 (test/is (= 6 (potential-energy {:pos [-1 2 -3]}))))

