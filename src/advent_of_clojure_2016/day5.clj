(ns advent-of-clojure-2016.day5
  (:require
   [clojure.java.io :as io]
   [digest :refer [md5]]
   [medley.core :refer [distinct-by]]))

(def input "ffykfhsq")

(defn find-password [code]
  (transduce
   (comp
    (map #(md5 (str code %)))
    (filter #(= "00000" (subs % 0 5)))
    (map #(nth % 5))
    (take 8))
   conj
   []
   (range)))

#_ (def res (time (find-password input)))
;; => [\c \6 \6 \9 \7 \b \5 \5]
;; 23.4 seconds baby!

(defn find-codes-2 [code]
  (transduce
   (comp
    (map #(md5 (str code %)))
    (filter #(and (= "00000" (subs % 0 5))
                  (> 8 (Integer/parseInt (subs % 5 6) 16))))
    (map (juxt #(Integer/parseInt (str (nth % 5)))
               #(nth % 6)))
    (distinct-by first)
    (take 8))
   conj
   []
   (range)))

(defn result-password [codes]
  (reduce #(assoc %1 (first %2) (second %2)) (vec (replicate 8 '_)) codes))

#_ (def res2 (time (result-password (find-codes-2 input))))
;; => [\8 \c \3 \5 \d \1 \a \b]
;; 110 seconds
