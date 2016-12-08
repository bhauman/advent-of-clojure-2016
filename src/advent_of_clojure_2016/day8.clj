(ns advent-of-clojure-2016.day8
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (line-seq (io/reader (io/resource "day8"))))

(def board (vec (repeat 6 (vec (repeat 50 '_)))))

(defn rect [b w h]
  (->> (for [x (range w) y (range h)] [y x])
       (reduce #(assoc-in %1 %2 'X) b)))

(def transpose #(apply mapv vector %))

(defn rotate [v a]
  (let [c (count v)
        r (- c (mod a c))]
    (vec (concat (drop r v) (take r v)))))

(defn rotate-row [b c a]
  (update-in b [c] rotate a))

(defn rotate-column [b c a]
  (-> b transpose (rotate-row c a) transpose))

(def to-ints (partial map #(Integer/parseInt %)))

(defn parse-rect-args [x] (to-ints (string/split x #"x")))

(defn parse-rotate-args [x] (to-ints [(subs (first x) 2) (last x)]))

(defn line-to-command [b line]
  (let [[x & xs] (string/split line #"\s")]
    (if (= "rect" x)
      (apply rect b (parse-rect-args (first xs)))
      (let [[x & xs] xs]
        (if (= "column" x)
          (apply rotate-column b (parse-rotate-args xs))
          (apply rotate-row    b (parse-rotate-args xs)))))))

;; part 1
#_(->> data
       (reduce line-to-command board)
       (apply concat)
       (filter #{'X})
       count)

;; part 2
#_(reduce line-to-command board data)
