(ns advent-of-clojure-2017.day01
  (:require [clojure.java.io :as io]))

(def data (mapv
           (comp read-string str)
           (slurp (io/resource "2017/day01"))))

(defn count-em [d]
  (->> (partition 2 1 (conj (vec d) (first d)))
       (filter (partial apply =))
       (map first)
       (apply +)))

(comment
  (count-em [1 1 2 2])
  (count-em [1 1 1 1])
  (count-em [1 2 3 4])
  (count-em [9 1 2 1 2 1 2 9]))

;; part 1
#_(count-em data)

(defn count-em2 [d]
  (->> (map
        vector
        d
        (drop (/ (clojure.core/count d) 2) (cycle d)))
       (filter (partial apply =))
       (map first)
       (apply +)))

(comment
  (count-em2 [1 2 1 2])
  (count-em2 [1 2 2 1])
  (count-em2 [1 2 3 4 2 5])
  (count-em2 [1 2 3 1 2 3])
  (count-em2 [1 2 1 3 1 4 1 5]))

;; part 2
#_(count-em2 data) 

