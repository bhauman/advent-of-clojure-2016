(ns advent-of-clojure-2017.day06
  (:require
   [clojure.java.io :as io]))

(def test-data [0 2 7 0])

(def data (read-string
           (str "[" (slurp (io/resource "2017/day06")) "]")))

(defn redistribute [block]
  (let [max-val  (apply max block)
        position (.indexOf block max-val)]
    (reduce #(update-in %1 [(mod %2 (count %1))] inc)
            (assoc block position 0)
            (range (inc position)
                   (+ (inc position) max-val)))))

; part 1
(defn part1 [first-block]
  (count (reduce
          (fn [history-set next-block]
            (if (history-set next-block)
              (reduced history-set)
              (conj history-set next-block)))
          #{}
          (iterate redistribute first-block))))

#_(part1 data)
;;=> 7864

;; part 2
(defn part2 [first-block]
  (reduce
   (fn [history-map next-block]
     (if-let [result (history-map next-block)]
       (reduced (- (count history-map) result))
       (assoc history-map next-block (count history-map))))
   {}
   (iterate redistribute first-block)))

#_(part2 data)
;;=> 1695









