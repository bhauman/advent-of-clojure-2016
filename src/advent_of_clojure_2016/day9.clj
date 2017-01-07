(ns advent-of-clojure-2016.day9
  (:require
   [advent-of-clojure-2016.utils :as u]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (string/trim (slurp (io/resource "day9"))))

(def directive-regex #"(\((\d+)x(\d+)\)).*")

(defn parse-dir [s]
  (when-let [[_ directive & args] (re-matches directive-regex (apply str s))]
    [(u/to-ints args) (drop (count directive) s)]))

(defn parse-directive [s]
  (when-let [[[cnt rpt] s] (parse-dir s)]
    [(take (* cnt rpt) (cycle (take cnt s))) (drop cnt s)]))

(defn starts-with-directive? [d]
  (re-matches directive-regex (apply str (take 20 d))))

(defn has-directive? [d]
  (re-find directive-regex (apply str d)))

(defn part1 [d]
  (loop [accum [] data d]
    (cond
      (empty? data) (apply str accum)
      (starts-with-directive? data)
      (let [[ac s] (parse-directive data)]
        (recur (concat accum ac) s))
      :else (recur (conj (vec accum) (first data)) (rest data)))))

;; part 1
#_(count (part1 data))
;; => 112830

(defn part2 [d]
  (loop [accum 0 data d]
    (cond
      (empty? data) accum
      (starts-with-directive? data)
      (when-let [[[cnt rpt] s] (parse-dir data)]
        (let [[part s] (split-at cnt s)]
          (recur (+ accum
                    (* rpt (if (has-directive? part) (part2 part) (count part))))
                 s)))
      :else (recur (inc accum) (rest data)))))

#_(part2 "(27x12)(20x12)(13x14)(7x10)(1x12)A")
#_(part2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")

#_(time (part2 data))

;; => 10931789799


