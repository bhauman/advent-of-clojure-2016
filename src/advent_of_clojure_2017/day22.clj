(ns advent-of-clojure-2017.day22
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data
  (->> (io/resource "2017/day22")
       io/reader
       line-seq
       vec))

(def test-data ["..#" "#.." "..."])

(defn start-board [data']
  (assert (= (count data') (count (first data'))))
  {:board
   (into {}
    (for [y (range (count data'))
          x (range (count data'))]
      [[y x] (str (get-in data' [y x]))]))
   ;; initial position center
   :position
   (let [x (int (/ (count data') 2))]
     [x x])
   ;; initial direction up
   :direction 0})

(def directions [[-1 0] [0 1] [1 0] [0 -1]])

(def turn-right  (comp #(mod % 4) inc))
(def turn-left   (comp #(mod % 4) dec))

(defn turn [{:keys [board position] :as state}]
  (->> (if (= "#" (get board position)) turn-right turn-left)
       (update state :direction)))

(defn infect-node [{:keys [board position] :as state}]
  (if (= "#" (get board position))
    (update state :board assoc position ".")
    (-> state
        (update :board assoc position "#")
        (update :infect-count (fnil inc 0)))))

(defn move-forward [{:keys [direction] :as state}]
  (update state :position #(mapv + % (get directions direction))))

(defn step [{:keys [board position direction] :as state}]
  (->> state turn infect-node move-forward))

;; we really have to render the board to verify that the state machine is working
(defn render-board [{:keys [board position]}]
  (let [min-coord (apply min (flatten (cons position (keys board))))
        max-coord (apply max (flatten (cons position (keys board))))]
    (->> (for [y (range min-coord (inc max-coord))
               x (range min-coord (inc max-coord))]
           [y x])
         (map #(if (= % position)
                 (str "[" (board % ".") "]")
                 (str " " (board % ".") " ")))
         (partition (- (inc max-coord) min-coord))
         (map #(apply str %))
         (mapv println))
    nil))

;; verify that everything looks right
#_(render-board (nth (iterate step (start-board test-data)) 7))

(defn count-bursts-of-activity [start-data n]
  (-> (iterate step (start-board start-data))
      (nth n)
      :infect-count))

;; part 1
#_ (time (count-bursts-of-activity data 10000))
;; 5240
;; Elapsed time: 41.665846 msecs

;; well ... we have a whole new state machine for part 2 woohoo!

(def state-transition
  {nil "W"
   "." "W"
   "W" "#"
   "#" "F"
   "F" "."})

(defn turn-2 [{:keys [board position] :as state}]
  (condp = (get board position ".")
    "." (update state :direction turn-left)
    "#" (update state :direction turn-right)
    "W" state
    "F" (update state :direction (comp turn-right turn-right))))

(defn update-node [{:keys [board position] :as state}]
  (cond-> state
    (= "W" (board position)) (update :infect-count (fnil inc 0))
    :else (update-in [:board position] state-transition)))

(defn step-2 [{:keys [position] :as state}]
  (-> state
      turn-2
      update-node
      move-forward))

;; render the board
#_(render-board (nth (iterate step-2 (start-board test-data)) 5))

(defn count-bursts-of-activity-2 [start-data n]
  (-> (iterate step-2 (start-board start-data))
      (nth n)
      :infect-count))

;; part 2
#_(time (count-bursts-of-activity-2 data 10000000))
;; => 2512144


