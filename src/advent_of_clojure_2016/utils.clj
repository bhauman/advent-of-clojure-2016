(ns advent-of-clojure-2016.utils)

(def transpose (partial apply mapv vector))

(defn rotate-left [v i]
  (let [c (count v)]
    (into [] (comp (drop (mod i c)) (take c)) (cycle v))))

(defn rotate-right [v i]
  (rotate-left v (- i)))

(defn insert-at [x n i]
  (let [[a b] (split-at n x)]
    (concat (conj (vec a) i) b)))

(defn delete-at [x n]
  (let [[a b] (split-at n x)]
    (concat a (rest b))))


(def to-int #(Integer/parseInt %))
(def to-ints (partial map to-int))

