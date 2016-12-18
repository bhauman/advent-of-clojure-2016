(ns advent-of-clojure-2016.day16)

(def input "11110010111001001")

(defn expand [a]
  (concat a (cons \0 (map {\1 \0 \0 \1} (reverse a)))))

(defn expand-until [size initial]
  (first (filter #(>= (count %) size) (iterate expand initial))))

(defn check-sum* [s]
  (map (fn [[a b]] (if (= a b) \1 \0)) (partition 2 s)))

(defn check-sum [s]
  (first (filter (comp odd? count) (iterate check-sum* s))))

(defn find-answer [size s]
  (->> (expand-until size s)
       (take size)
       check-sum
       (apply str)))

#_(find-answer 272 input)
;; => 01110011101111011

;; not performant (222 seconds) but 35Megs isn't impossible
;; structural sharing and seqs perform their magic here
#_ (def res (time (find-answer 35651584 input)))
;; => 11001111011000111

