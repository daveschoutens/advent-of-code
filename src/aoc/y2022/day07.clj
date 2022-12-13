(ns aoc.y2022.day07
  (:require [clojure.zip :as z]))

(defn terminal-command
  [[command & output]]
  (let [[_ command arg] (re-matches #"(\w+) ?(.+)?" command)]
    {:command command :arg arg :output output}))

(defn terminal-commands
  [terminal-session]
  (->> (clojure.string/split terminal-session #"\$ ")
       (map clojure.string/split-lines)
       rest
       (map terminal-command)))

(defn file [ls-output]
  (let [[_ x y] (re-matches #"(dir|\d+) (.+)" ls-output)]
    (if (= "dir" x)
      {:type :dir :name y :contents []}
      {:type :file :name y :size (parse-long x)})))

(defn fs-zipper [fs]
  (z/zipper
   (fn branch? [x] (= :dir (:type x)))
   (fn children [x] (:contents x))
   (fn make-node [x children] (assoc x :contents children))
   fs))

(defn find-child-dir
  [fs-zipper dirname]
  (first (filter #(-> % z/node :name (= dirname))
                 (take-while some? (iterate z/right (z/down fs-zipper))))))

(defn filesystem
  [terminal-commands]
  (z/root (reduce
           (fn [fs {:keys [command arg output]}]
             (case command
               "cd" (case arg
                      "/" (fs-zipper (z/root fs))
                      ".." (z/up fs)
                      (find-child-dir fs arg))
               "ls" (reduce (fn [fs item] (z/insert-child fs (file item))) fs output)))
           (fs-zipper {:type :dir :name "/" :contents '()})
           terminal-commands)))

(defn iter-zip [zipper]
  (->> zipper (iterate z/next) (take-while (complement z/end?))))

(defn all-dirs-with-size [fs]
  (for [n (iter-zip (fs-zipper fs))
        :let [{:keys [type] :as node} (z/node n)]
        :when (= :dir type)]
    (assoc node :size
           (reduce
            (fn [t f] (+ t (get (z/node f) :size 0)))
            0
            (iter-zip (fs-zipper node))))))

(defn make-filesystem [input]
  (->> input terminal-commands filesystem))

(defn solve-1
  ([] (solve-1 (slurp "input/2022day07")))
  ([input]
   (let [fs (make-filesystem input)]
     (->> (all-dirs-with-size fs)
          (map :size)
          (filter #(>= 100000 %))
          (reduce +)))))

(defn solve-2
  ([] (solve-2 (slurp "input/2022day07")))
  ([input]
   (let [fs (make-filesystem input)
         du (all-dirs-with-size fs)
         total-size (:size (first (filter #(= "/" (:name %)) du)))
         unused-space (- 70000000 total-size)
         needed-space (- 30000000 unused-space)]
     (->> du
          (map :size)
          (filter #(< needed-space %))
          sort
          first))))

(comment
  (solve-1)
  (solve-2))
