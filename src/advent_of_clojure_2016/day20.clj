(ns advent-of-clojure-2016.day20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [advent-of-clojure-2016.utils :as u]))

(def data
  (->> (slurp (io/resource "day20"))
      string/split-lines
      (map #(string/split % #"-"))
      (map #(mapv biginteger %))
      (mapv #(do (assert (apply < %)) %))
      sort))

(defn overlap? [[[l h] [l2 h2]]] (>= (inc h) l2))

(defn merge-em [a b]
  (let [[[l h] [l2 h2]] (sort [a b])]
    [(min l l2) (max h h2)]))

(defn merge-all [ips]
  (reduce (fn [[accum & xs :as st] nxt]
            (if (overlap? accum nxt)
              (cons (merge-em accum nxt) xs)
              (cons nxt st))) 
          [[0 0]]
          ips))

;; part 1
(-> (merge-all data) last last inc)
;; => 4793564

(defn size [[l h]] (inc (- h l)))

;; part 2
(->> (merge-all data)
     (map size)
     (reduce +)
     (- (inc (biginteger "4294967295"))))
;; => 146
