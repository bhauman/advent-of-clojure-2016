(ns advent-of-clojure-2017.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (->>
           (io/resource "2017/day02")
           io/reader
           line-seq
           (mapv (comp (partial mapv read-string)
                       #(string/split % #"\s+")))))

(defn checksum-1 [row]
  (- (apply max row) (apply min row)))

(and
  (= 8 (checksum-1 [5 1 9 5]))
  (= 4 (checksum-1 [7 5 3]))
  (= 6 (checksum-1 [2 4 6 8])))

;; part 1
#_(apply + (map checksum-1 data))

(defn checksum-2 [row]
  (first (for [a row
               b row
               :when (and (not= a b)
                          (= 0 (rem a b)))]
           (/ a b))))

(and
  (= (checksum-2 [5 9 2 8]) 4)
  (= (checksum-2 [9 4 7 3]) 3)
  (= (checksum-2 [3 8 6 5]) 2))

;; part 2
#_(apply + (map checksum-2 data))

