(ns advent-of-clojure-2017.day20
  (:require
   [clojure.java.io :as io]))

(def data
  (->> (io/resource "2017/day20")
       io/reader
       line-seq
       (map #(re-seq #"[-]*\d+" %))
       (map #(map read-string %))
       (map (comp vec (partial partition 3)))))

(defn val-vec [n]
  (if (number? n) [n n n] n))

(def vec* (partial mapv *))
(def vec+ (partial mapv +))

(defn position-at-time [[start-position start-velocity acceleration] t]
  ;; 1/2 a t^2 + v t + c
  (vec+ (vec*
         acceleration
         (val-vec (* 0.5 t t)))
        (vec* start-velocity (val-vec t))
        start-position))

(defn distance-at-time [t particle]
  (apply + (map #(Math/abs %) (position-at-time particle t))))

;; part 1
#_ (->> data
        (map (juxt identity (partial distance-at-time 1000)))
        (sort-by second)
        ffirst
        (.indexOf data)
        time)
;; => 150
;; Elapsed time: 32.838903 msecs

(defn particle-step [[position velocity acceleration :as particle]]
  (let [v (vec+ velocity acceleration)]
    [(vec+ position v) v acceleration]))

(defn step [state]
  (->> state
       (mapv particle-step)
       (group-by first)
       (filter #(= 1 (count (second %))))
       (mapv (comp first second))))

;; part 2
(->> data
     (iterate step)
     (drop 100)
     first
     count
     time)
;; => 657
