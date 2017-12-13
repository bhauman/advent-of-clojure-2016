(ns advent-of-clojure-2017.day12
  (:require
   [clojure.java.io :as io]
   
   [clojure.set :as st]))

(def data (->> (io/resource "2017/day12")
               io/reader
               line-seq
               (map #(format "[%s]" %))
               (map read-string)))

(defn index [data']
  (reduce (fn [accum [node _ & direct-connect]]
            (as-> (set direct-connect) x
              (disj x node)
              (assoc accum node x)))
          {}
          data'))

;; find group without cycles

(defn group [idx root]
  (into #{}
        (tree-seq (let [seen (atom #{})]
                    (fn [x] (when-not (@seen x)
                              (swap! seen conj x))))
                  idx
                  root)))

;; part 1
#_(let [idx (index data)]
    (time (count (group idx 0))))
;; => 113

;; part 2
;; not worth optimizing for problem space
#_(let [idx (index data)]
    (->> (map #(group idx %) (keys idx))
         (into #{})
         count))
;; => 202



