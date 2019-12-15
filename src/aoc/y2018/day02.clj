(ns aoc.y2018.day02)

(defn input->data [input] (clojure.string/split input #"\n"))

(defn pairs-and-triples [box-id]
  (let [freqs (vals (frequencies box-id))
        has-pair? (some #{2} freqs)
        has-triple? (some #{3} freqs)]
    [has-pair? has-triple?]))

(defn checksum [box-ids]
  (let [pts (->> box-ids (map pairs-and-triples))
        all-pairs (->> pts (map first) (remove nil?) count)
        all-triples (->> pts (map second) (remove nil?) count)]
    (* all-pairs all-triples)))

(defn solve-1
  ([] (solve-1 (slurp "input/2018day02")))
  ([input] (checksum (input->data input))))

(defn remove-indexed [i coll]
  (concat (take i coll) (nthrest coll (inc i))))

(defn remove-indexed-char [i s]
  (str (subs s 0 i) (subs s (inc i))))

(defn remove-indexed-char-from-all [strs index]
  (map (partial remove-indexed-char index) strs))

(defn find-duplicate [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(defn solve-2 
  ([] (solve-2 (slurp "input/2018day02")))
  ([input] 
   (first (mapcat 
           #(find-duplicate (remove-indexed-char-from-all (input->data input) %))
           (range)))))