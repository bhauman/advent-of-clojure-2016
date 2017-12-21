(ns advent-of-clojure-2017.day21
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def transpose (partial apply map vector))
(def flip (partial map reverse))
(def rotate-right (comp flip transpose))

(defn all-rotations [rule-key]
  (let [rotations (take 4 (iterate rotate-right (map seq rule-key)))]
    (concat rotations (map flip rotations))))

(defn expand-rule [rules [rule-key rule-val]]
  (->> (repeat (map seq rule-val))
       (map vector (all-rotations rule-key))
       (into {})
       (merge rules)))

(defn parse-rules [lines]
  (->> lines
       (map #(string/split % #" => "))
       (map (partial map #(string/split % #"/")))
       (reduce expand-rule {})))

(def rules
  (->> (io/resource "2017/day21")
       io/reader
       line-seq
       parse-rules))

(def start-pixels '((\. \# \.)
                    (\. \. \#)
                    (\# \# \#)))

(defn break-into [grid n]
  (map #(map transpose (partition n (transpose %)))
       (partition n grid)))

(defn normalize [broken-out]
  (mapcat #(map flatten (transpose %)) broken-out))

(defn apply-rules [rules grid]
  (->> (let [height (count grid)]
         (cond
           (zero? (mod height 2)) 2
           (zero? (mod height 3)) 3))
       (break-into grid)
       (map #(map rules %))
       normalize))

(defn pixels-on-at [n start-pixels]
  (->> (nth (iterate (partial apply-rules rules) start-pixels) n)
       flatten
       (filter #{\#})
       count))

;; part 1
#_ (pixels-on-at 5 start-pixels)
;; => 139

;; part 2
#_ (pixels-on-at 18 start-pixels)
;; => 1857134


