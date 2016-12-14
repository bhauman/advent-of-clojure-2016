(ns advent-of-clojure-2016.day13
  (:require
   [clojure.string :as string]
   [clojure.set :refer [union]]))

(def ^:dynamic *input*  1352)
(def ^:dynamic *bound*  41)
(def ^:dynamic *target* [31 39])
(def ^:dynamic *upper-bound* 100)

(defn open? [[x y]]
  (->> (+ *input* (* x x) (* 3 x) (* 2 x y) y (* y y))
       Integer/toBinaryString
       (filter #(= \1 %))
       count
       even?))

(defn view-structure [sz]
  (for [y (range sz)]
    (for [x (range sz)]
      (cond
        (= *target* [x y]) 'O
        (open? [x y]) '.
        :else '=))))

(defn next-steps [path-set coord]
  (sequence
   (comp
    (map (partial mapv + coord))
    (filter #(every? (fn [i] (<= 0 i *bound*)) %))
    (filter (complement path-set))
    (filter open?))
   [[1 0] [-1 0] [0 1] [0 -1]]))

(defn depth-search [path-set coord]
  (cond
    (= coord *target*) (count path-set)
    (> (count path-set) *upper-bound*) Integer/MAX_VALUE 
    :else
    (if-let [res (->> (next-steps path-set coord)
                      (map #(depth-search (conj path-set coord) %))
                      not-empty)]
      (reduce min res)
      Integer/MAX_VALUE)))

#_(view-structure 41)

;; sample input
#_(depth-search #{} [1 1])
;; => 90

(defn collect-positions [path-set coord]
  (if (>= (count path-set) *upper-bound*)
    #{coord}
    (->> (next-steps path-set coord)
         (map #(collect-positions (conj path-set coord) %))
         (apply union #{coord}))))

;; part 2
#_(binding [*upper-bound* 50]
    (count (collect-positions #{} [1 1])))
;; => 135
