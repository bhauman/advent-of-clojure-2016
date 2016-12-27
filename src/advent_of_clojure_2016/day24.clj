(ns advent-eof-clojure-2016.day24
  (:require
   [advent-of-clojure-2016.utils :as u]
   [clojure.java.io :as io]
   [clojure.set :refer [difference]]
   [medley.core :refer [distinct-by]]
   [clojure.string :as string]))

;; travelling salesman problem

(defn make-map [s]
  (->>
   (for [[y line] (map-indexed vector (string/split-lines s))
         [x chr]  (map-indexed vector line)]
     [[x y] chr])
   (into {})))

(defn number-positions [m]
  (->> (filter #(re-matches #"\d" (str (second %))) m)
       (map (comp vec reverse))
       (map #(update % 0 (comp u/to-int str)))
       (into {})))

(defn open-closed [m]
  (into {}
        (keep (fn [[k v]] (when (not= \# v) [k v])) m)))

(defn make-map-state [s]
  (let [m (make-map s)]
    {:positions (open-closed m)
     :number-location (number-positions m)
     :location-number (into {}
                             (map (comp vec reverse)
                                  (number-positions m)))}))

(def input (make-map-state (slurp (io/resource "day24"))))

(defn possible-moves [{:keys [moves-so-far positions current-position]}]
  (let [nm (map #(mapv + current-position %) (shuffle [[0 1] [0 -1] [1 0] [-1 0]]))]
    (filter #(and (not (moves-so-far %)) (positions %)) nm)))

(defn at-number? [{:keys [current-position location-number]}]
  (location-number current-position))

(defn make-move [st move]
  (let [r (-> st
              (update :moves-so-far conj (:current-position st))
              (assoc :current-position move))]
    (assoc r :at-number (at-number? r))))

(defn next-level [sts]
  (->> (mapcat #(map (partial make-move %) (possible-moves %)) sts)
       (distinct-by :current-position)
       (sort-by #(if (at-number? %) 0 1))))

(defn breadth-first-search [st]
  (drop 1 (iterate next-level [st])))

(defn distances-from-position [st a]
  (->> (breadth-first-search (assoc st
                                    :current-position (get-in st [:number-location a])
                                    :moves-so-far #{}))
       (mapcat #(filter :at-number %))
       (distinct-by :at-number)
       (take (dec (count (:location-number st))))
       (map #(vector #{a (:at-number %)} (count (:moves-so-far %))))
       (into {})))

(defn get-all-distances [input]
  (->> (map (partial distances-from-position input) (range 8))
       (reduce (partial merge-with min) {})))

(def all-distances (time (get-all-distances input)))

(defn travelling-salesmen [so-far available distances]
  (if (= 8 (count so-far))
    0
    (let [lst (first so-far)]
      (reduce min
              (map (fn [x]
                     (+ (get distances #{lst x})
                        (travelling-salesmen (cons x so-far) available distances)))
                   (difference (set available) (set so-far)))))))

;; part 1
#_(travelling-salesmen [0] (range 8) all-distances)
;; => 456

(defn travelling-salesmen-part-2 [so-far available distances]
  (if (= 8 (count so-far))
    (get distances #{(first so-far) 0})
    (let [lst (first so-far)]
      (reduce min
              (map (fn [x]
                     (+ (get distances #{lst x})
                        (travelling-salesmen-part-2
                         (cons x so-far) available distances)))
                   (difference (set available) (set so-far)))))))

;; part 2
#_(travelling-salesmen-part-2 [0] (range 8) all-distances)
;; => 704




