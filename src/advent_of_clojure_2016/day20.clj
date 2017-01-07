(ns advent-of-clojure-2016.day20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data
  (->> (slurp (io/resource "day20"))
       string/split-lines
       (map #(string/split % #"-"))
       (map #(mapv read-string %))
       (mapv #(do (assert (apply < %)) %))
       sort))

(defn overlap? [[l h] [l2 h2]] (>= (inc h) l2))

(defn merge-em [[l h] [l2 h2]] [(min l l2) (max h h2)])

(defn merge-all [ips]
  (reduce (fn [[accum & xs :as st] nxt]
            (if (overlap? accum nxt)
              (cons (merge-em accum nxt) xs)
              (cons nxt st))) 
          [[0 0]]
          ips))

;; part 1
#_(-> (merge-all data) last last inc)
;; => 4793564

(defn size [[l h]] (inc (- h l)))

;; part 2
#_(->> (merge-all data)
       (map size)
       (reduce +)
       (- (size [0 4294967295])))
;; => 146
