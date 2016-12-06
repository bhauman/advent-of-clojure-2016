(ns advent-of-clojure-2016.day1
  (:require
   [clojure.java.io :as io]))

(def data (read-string (str "[" (slurp (io/resource "day1")) "]")))

(defn parse-data [d]
  (->> d
       (map name)
       (map (juxt first
                  #(Integer/parseInt (apply str (rest %)))))))

(defn positions [d]
  (->> (map first d)
       (reductions #(({\L dec \R inc} %2) %1) 0)
       rest
       (map #(mod % 4))
       (map [[0 1] [1 0] [0 -1] [-1 0]])
       (mapcat repeat (map second d))
       (reductions (partial map +) (list 0 0))))

(defn point-to-dist [p]
  (->> p
       (map #(Math/abs %))
       (reduce +)))

;; part 1
#_(->> data
       parse-data
       positions
       last
       point-to-dist)

;; part 2
#_(->> data
       parse-data
       positions
       (reductions conj (list))
       (filter (fn [[x & xs]] ((set xs) x)))
       ffirst
       point-to-dist)
