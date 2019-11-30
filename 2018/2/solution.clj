(defn read-input []
  (clojure.string/split (slurp "input") #"\n"))

(defn pairs-and-triples [box-id]
  (let [freqs (vals (frequencies box-id))
        has-pair? (some #{2} freqs)
        has-triple? (some #{3} freqs)]
    [has-pair? has-triple?]))

(defn checksum [box-ids]
  (let [pts (->> box-ids (map pairs-and-triples))
        all-pairs (->> pts (map first) (remove nil?) count)
        all-triples (->> pts (map second) (remove nil?) count)]
    (do
      (print all-pairs)
      (print all-triples)
      (* all-pairs all-triples))))

;; Solution one
(checksum (read-input))

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

;; Solution two
(first (mapcat #(find-duplicate (remove-indexed-char-from-all (read-input) %)) (range)))