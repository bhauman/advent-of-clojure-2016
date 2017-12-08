(ns advent-of-clojure-2017.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-data [lines]
  (->> lines
       (map #(str "[" % "]"))
       (map read-string)))

(def data (->> (io/resource "2017/day08")
               io/reader
               line-seq
               parse-data))

(def fn-map
  (->> (map (juxt identity eval)'(> < >= <= ==))
       (into {'inc (fnil + 0)
              'dec (fnil - 0)
              '!= not=})))

(defn eval-exp [accum [reg inc-dec value _ cond-reg pred pred-val]]
  (if ((fn-map pred) (get accum cond-reg 0) pred-val)
    (update accum reg (fn-map inc-dec) value)
    accum))

(defn part1 [instructions]
  (->> (reduce eval-exp {} instructions)
       vals
       (sort >)
       first))

;; part 1
#_(part1 data)
;; => 5075

(defn part2 [instructions]
  (->> (reductions eval-exp {} instructions)
       (keep (comp first (partial sort >) vals))
       (sort >)
       first))

;; part 2
#_(part2 data)
;; => 7310


(comment
  (def test-data
    (-> "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"
      string/split-lines
      parse-data))
  )
