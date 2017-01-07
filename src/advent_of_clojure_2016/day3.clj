(ns advent-of-clojure-2016.day3
  (:require
   [clojure.java.io :as io]))

(def lines (line-seq (io/reader (io/resource "day3"))))

(defn parse-triangle [line]
  (read-string (str "[" line "]")))

(defn valid? [[a b c]]
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ c a) b)))

;; part 1
#_(->> lines
       (map parse-triangle)
       (filter valid?)
       count)
;; => 862

(def transpose (partial apply mapv vector))

;; part 2
#_(->> lines
       (map parse-triangle)
       (partition 3)
       (mapcat transpose)
       (filter valid?)
       count)
;; => 1577
