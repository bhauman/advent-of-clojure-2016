(ns advent-of-clojure-2016.day22
  (:require
   [advent-of-clojure-2016.utils :as u]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :refer [distinct-by]]))

(defn parse-input [s]
  (into (sorted-map)
        (comp
         (drop 2)
         (map #(re-seq #"\d+" %))
         (map u/to-ints)
         (map #(take 4 %))
         (map #(partition 2 %))
         (map #(mapv vec %)))
        (string/split-lines s)))

(def data (parse-input (slurp (io/resource "day22"))))

(def available (partial apply -))
(def size first)
(def used second)
(def empty-node (comp zero? used))

(defn viable-pair? [a b]
  (and a b
       (not (empty-node a))
       (>= (available b) (used a))))

;; part 1
#_(count
 (for [[pos1 data1] data
       [pos2 data2] data
       :when (not= pos1 pos2)
       :when (viable-pair? data1 data2)]
   [pos1 pos2]))
;; => 1045

(defn view-grid [{:keys [data]}]
  (apply map vector (partition 30 (map used (vals data)))))

(defn pp [d]
  (binding [clojure.pprint/*print-right-margin* 200]
    (clojure.pprint/pprint d)))

(defn make-initial [data]
  {:data data
   :g [(->> data keys (map first) (reduce max)) 0]
   :last-move [(ffirst (filter (comp empty-node second) data))]})

;; this is subtle: a preference for up and to the left is helpful
;; as it provides the very last disambiguation between = score moves
(defn connections [a]
  (map #(mapv + a %) [[-1 0] [0 -1] [0 1] [1 0]]))

(defn possible-moves [{:keys [last-move data]}]
  (let [to-pos  (first last-move)
        to-data (get data to-pos)]
    (for [from-pos (connections to-pos)
          :let [from-data (get data from-pos)]
          :when (viable-pair? from-data to-data)]
      [from-pos to-pos])))

(defn make-move [{:keys [data g] :as st} [from to]]
  (let [from-data (get data from)]
    (-> st
        (update-in [:data to 1] + (used from-data))
        (assoc-in  [:data from 1] 0)
        (assoc :g  (if (= g from) to g)
               :last-move [from to]))))

(defn next-states [{:keys [last-move] :as st}]
  (->> (possible-moves st)
       (filter #(not= last-move (reverse %)))
       (map (partial make-move st))))

(defn distance [from to]
  (if (and from to)
    (apply + (map #(Math/abs %) (map - from to)))
    Integer/MAX_VALUE))

(defn score [{:keys [g last-move]}]
  [(apply + g)
   (distance g (first last-move))])

(defn next-level [state]
  (first (sort-by score (next-states state))))

(defn find-answer2 [limit data]
  (->> (iterate next-level (make-initial data))
       (take-while #(not= (:g %) [0 0]))
       (take limit)
       count))

#_(time (find-answer2 500 data))
;; => 265
