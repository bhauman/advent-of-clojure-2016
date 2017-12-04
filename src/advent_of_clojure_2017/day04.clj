(ns advent-of-clojure-2017.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as m]))

(def data
  (->> (io/resource "2017/day04")
       io/reader
       line-seq
       (mapv #(string/split % #"\s"))))

;; part 1
#_(count (filter #(= (count (distinct %)) (count %))
                 data))

(defn word-signature [word]
  (reduce #(merge-with (fnil + 0) %1 {%2 1}) {} word))

#_(count (filter #(= (count (m/distinct-by word-signature %)) (count %))
                 data))
