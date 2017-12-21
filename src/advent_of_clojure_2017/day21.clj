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
       count
       time))

;; part 1
#_ (pixels-on-at 5 start-pixels)
;; => 139
;; Elapsed time: 1.537593 msecs

;; part 2
#_ (pixels-on-at 18 start-pixels)
;; => 1857134
;; Elapsed time: 19130.777919 msecs


;; memoized recursive solution to part 2, it should be much faster

(declare count-at-depth)

(defn count-at-depth-helper [depth grid]
  (->> (break-into grid 2)
       (apply concat)
       (map rules)
       (map #(count-at-depth (dec depth) %))
       (reduce +)))

(defn count-at-depth [depth grid]
  (if (zero? depth)
    (count (filter #{\#} (flatten grid)))
    (condp = (count grid)
      2 (count-at-depth (dec depth) (rules grid))
      3 (cond
          ;; jump over ambiguity
          (>= depth 2)
          (count-at-depth (- depth 2)
                          (->> grid
                               rules
                               (apply-rules rules)))
          ;; end of road
          (= depth 1)
          (count-at-depth (dec depth) (rules grid)))
      4 (count-at-depth-helper depth grid)
      6 (count-at-depth-helper depth grid))))

;; part 2
#_(with-redefs [count-at-depth (memoize count-at-depth)]
    (time (count-at-depth 18 start-pixels)))
;; => 1857134
;; Elapsed time: 5.832728 msecs
