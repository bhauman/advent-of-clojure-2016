(ns advent-of-clojure-2017.day19
  (:require
   [clojure.java.io :as io]))

(def data
  (->> (io/resource "2017/day19")
       io/reader
       line-seq
       vec))

(def directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn letter? [char']
  (<= (int \A) (int char') (int \Z)))

(defn char-at-position [char-map [y x]]
  (.charAt ^String (get char-map y) x))

(defn starting-x [char-map]
  (.indexOf (first char-map) (int \|)))

(defn next-directions [char-map [pos current-direction]]
  (if (not= \+ (char-at-position char-map pos))
    [current-direction]
    (let [opposite-direction (map - current-direction)]
      (filter #(not= % opposite-direction) directions))))

(defn next-positions [char-map [pos :as state]]
  (->> (next-directions char-map state)
       (mapv (partial mapv + pos))
       (filter #(not= \space (char-at-position char-map %)))))

(defn move [char-map [pos :as state]]
  (assert (not= \space (char-at-position char-map pos)))
  (let [candidate-positions (next-positions char-map state)]
    (assert (#{0 1} (count candidate-positions)))
    (when-let [new-pos (first candidate-positions)]
        [new-pos (mapv - new-pos pos)])))

#_(->> (iterate (partial move data) [[0 (starting-x data)] [1 0]])
       (take-while identity)
       (map first)
       (map (partial char-at-position data))
       (filter letter?)
       (apply str)
       time)
;; part 1
;; => EOCZQMURF
;; Elapsed time: 117.173515 msecs

;; part 2
#_(->> (iterate (partial move data)
                [[0 (starting-x data)]
                 [1 0]])
       (take-while identity)
       count
       time)
;; => 16312
;; Elapsed time: 98.812626 msecs
