(ns advent-of-clojure-2016.day18)

(def input
  (str "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^"
       ".....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"))

(def t? #(= \^ %))

(def nt? (complement t?))

(defn next-trap? [[l c r]]
  (if (or (and (t? l) (nt? c) (nt? r))
          (and (nt? l) (nt? c) (t? r))
          (and (t? l) (t? c) (nt? r))
          (and (nt? l) (t? c) (t? r)))
    \^ \.))

(defn next-row [r]
  (->> (cons \. (conj r \.))
       (partition 3 1)
       (mapv next-trap?)))

(defn find-answer [rows r]
  (->> (iterate next-row (vec r))
       (take rows)
       (pmap #(count (filter nt? %)))
       (reduce +)))

;; part 1
#_(find-answer 40 input)
;; => 1982

;; part 2
#_ (time (find-answer 400000 input))
;; => 20005203
