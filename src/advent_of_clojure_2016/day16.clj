(ns advent-of-clojure-2016.day16)

(defn expand [a]
  (concat a (cons \0 (map {\1 \0 \0 \1} (reverse a)))))

(defn expand-until [size initial]
  (take size
        (first (filter #(>= (count %) size) (iterate expand initial)))))

(defn check-sum* [s]
  (map (fn [[a b]] (if (= a b) \1 \0)) (partition 2 s)))

(defn check-sum [s]
  (first (filter (comp odd? count) (iterate check-sum* s))))

(def find-answer (comp (partial apply str) check-sum expand-until))

#_(find-answer 272 "11110010111001001")
;; => 01110011101111011

; not performant (222 seconds) but 35Megs isn't impossible
; structural sharing and seqs perform their magic here
#_ (def res  (time (find-answer 35651584 "11110010111001001")))
;; => 11001111011000111

