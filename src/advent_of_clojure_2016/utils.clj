(ns advent-of-clojure-2016.utils)

(defn log [x] (println "log: " (pr-str x)) x)

(def transpose (partial apply mapv vector))

(defn rotate-left [v i]
  (let [c (count v)]
    (into [] (comp (drop (mod i c)) (take c)) (cycle v))))

(defn rotate-right [v i]
  (rotate-left v (- i)))

(defn insert-at [x n i]
  (let [[a b] (split-at n x)]
    (into (conj (vec a) i) b)))

(defn delete-at [x n]
  (let [[a b] (split-at n x)]
    (into (vec a) (rest b))))

(defn pluck [pred list]
  [(filter pred list) (remove pred list)])

(def to-int #(Integer/parseInt %))
(def to-ints (partial map to-int))

(defn apply-times [n f init]
  (loop [c 0 h init] (if (= c n) h (recur (inc c) (f h)))))

(defn indexes-by [f coll]
  (sequence
   (comp
    (map-indexed vector)
    (filter (comp f second))
    (map first))
   coll))
