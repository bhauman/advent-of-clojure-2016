(ns advent-of-clojure-2017.day03
  (:require
   [clojure.core.reducers :as r]))

(def directions [[0 -1] [1 0] [0 1] [-1 0]])

;; a lazy list of all the directions travelled in order
;; from the start of the spiral
(def spiral-direction-list
  (for [step (range)
        stage (range (* 2 step))
        :let [direction-total
              (- (* 2 step)
                 (if (< stage step) 1 0))]]
    (mod direction-total 4)))

(defn next-position [pos direction]
  (mapv + pos (directions direction)))

(def spiral-positions-list
  (reductions next-position [0 0] spiral-direction-list))

(defn position-at-index [index]
  (nth spiral-positions-list (dec index)))

;; part 1
#_(->> (position-at-index 347991)
       (map #(Math/abs %))
       (apply +))

(def all-directions
  (into [[-1 -1] [-1 1] [1 -1] [1 1]]
        directions))

(defn all-neigbor-positions [pos]
  (mapv #(mapv + pos %) all-directions))

(defn neigbor-values [pos positions]
  (keep positions (all-neigbor-positions pos)))

(defn state-transition [{:keys [positions] :as state} next-pos]
  (let [value (apply + (neigbor-values next-pos positions))]
    (-> state
        (assoc    :value value)
        (assoc-in [:positions next-pos] value))))

(def neighbor-value-list
  (map :value
       (reductions
        state-transition
        {:value 1
         :positions {[0 0] 1}}
        (rest spiral-positions-list))))

;; part 2

#_(->> neighbor-value-list
       (filter #(> % 347991))
       first)


