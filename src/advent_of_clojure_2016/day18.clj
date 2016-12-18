(ns advent-of-clojure-2016.day18)

(def input
  (str "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^"
       ".....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"))

(def t? #{"^^." ".^^" "^.." "..^"})

(defn next-trap? [x]
  (if (t? (apply str x)) \^ \.))

(defn next-row [r]
  (->> (cons \. (conj r \.))
       (partition 3 1)
       (mapv next-trap?)))

(defn find-answer [rows r]
  (->> (iterate next-row (vec r))
       (pmap #(count (filter #{\.} %)))
       (take rows)
       (reduce +)))

;; part 1
#_(find-answer 40 input)
;; => 1982

;; part 2
#_ (time (find-answer 400000 input))
;; => 20005203
