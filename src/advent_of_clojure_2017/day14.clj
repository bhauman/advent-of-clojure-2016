(ns advent-of-clojure-2017.day14
  (:require
   [advent-of-clojure-2017.day10 :refer [sparse-hash dense-hash]]))

(def test-input "flqrgnkx")

(def puzzle-input "oundnydw")

(defn binary-dense-hash [sparseh]
  (->> sparseh
       (partition 16)
       (map #(reduce bit-xor %))
       (map #(Integer/toBinaryString %))
       (map #(format "%8s" %))
       (apply str)))

(def knot-hash (comp binary-dense-hash sparse-hash (partial map int)))

(defn disk-sector-map [input]
  (->> (map #(str %1 "-" %2) (repeat input) (range 128))
       (mapv knot-hash)))

;; part 1
#_(->> (disk-sector-map puzzle-input)
       (map (comp count (partial filter #{\1})))
       (reduce +)
       time)
;; Elapsed time: 3068.830745 msec
;; => 8106

(def data (->> (disk-sector-map puzzle-input)
               (mapv (partial mapv {\1 1 \space 0 \0 0}))))

(def directions [[0 1] [0 -1] [1 0] [-1 0]])

(defn pos->directions [pos]
  (->> (mapv #(mapv + pos %)  directions)
       ;; not really needed but saves a few steps
       (filter (fn [[y x]] (and
                            (< -1 y 128)
                            (< -1 y 128))))))

(defn children [data pos]
  (doall
   (filter #(= 1 (get-in data %))
           (pos->directions pos))))

(defn group [data children-fn pos]
  (set (tree-seq
         (let [seen (atom #{})]
           (fn [x] (when-not (@seen x)
                     (do
                       (swap! seen conj x)
                       (and
                        (not= 0 (get-in data x))
                        (not-empty (children-fn x)))))))
         children-fn
         pos)))

(defn all-groups [data]
  (let [children-fn (memoize (partial children data))]
    (into #{}
          (for [x (range 128)
                y (range 128)
                :let [pos [y x]]
                :when (not (zero? (get-in data pos)))]
            (group data children-fn pos)))))

;; part 2
#_(time (count (all-groups data)))
;; Elapsed time: 3957.949806 msecs
;; => 1164

