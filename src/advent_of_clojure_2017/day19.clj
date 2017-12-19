(ns advent-of-clojure-2017.day19
  (:require
   [clojure.java.io :as io]))

(def data
  (->> (io/resource "2017/day19")
       io/reader
       line-seq
       (mapv vec)))

(def directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn letter? [char']
  (<= (int \A) (int char') (int \Z)))

(defn starting-x [char-map]
  (.indexOf (first char-map) \|))

(defn next-directions [char-map [pos prev-pos]]
  (if (not= \+ (get-in char-map pos))
    [(mapv - pos prev-pos)]
    directions))

(defn next-position [char-map [pos prev-pos :as state]]
  (->> (next-directions char-map state)
       (mapv (partial mapv + pos))
       (filter #(not= prev-pos %))
       (filter #(not= \space (get-in char-map %)))
       first))

(defn move [char-map [pos :as state]]
  [(next-position char-map state) pos])

#_(->> (iterate (partial move data)
                (let [start-x (starting-x data)]
                  [[0 start-x] [-1 start-x]]))
       (map first)
       (take-while identity)
       (map (partial get-in data))
       (filter letter?)
       (apply str)
       time)
;; part 1
;; => EOCZQMURF
;; Elapsed time: 80.573515 msecs

;; part 2
#_(->> (iterate (partial move data)
                (let [start-x (starting-x data)]
                  [[0 start-x] [-1 start-x]]))
       (map first)
       (take-while identity)
       count
       time)
;; => 16312
;; Elapsed time: 72.912626 msecs
