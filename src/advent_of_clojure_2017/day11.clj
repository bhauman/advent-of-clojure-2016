(ns advent-of-clojure-2017.day11
  (:require
   [clojure.java.io :as io]))

(def data (read-string (format "[%s]" (slurp (io/resource "2017/day11")))))

;ne,ne,ne is 3 steps away.
;ne,ne,sw,sw is 0 steps away (back where you started).
;ne,ne,s,s is 2 steps away (se,se).
;se,sw,se,sw,sw is 3 steps away (s,s,sw).

;; this can be represented in terms of x and y coords
;; with a single diagonal path in each square
;; *-*-*-*-*-*
;; |/|/|/|/|/|
;; *-*-*-*-*-*
;; |/|/|/|/|/|
;; *-*-*-*-*-*
;; |/|/|/|/|/|
;; *-*-*-*-*-*
;; us regular x y coords and increment both when travelling
;; on the diagonal

(def directions
  {'n [1 1]
   's [-1 -1]
   'nw [0 1]
   'ne [1 0]
   'se [0 -1]
   'sw [-1 0]})

;; the distance formula is (max x y) if the point lies in
;; the direction of the diagonals

;; the distance formula is (+ x y) if the point lies against
;; the direction of the diagonals

(defn distance [coord]
  (if (or (every? pos? coord) (every? neg? coord))
    (apply max (mapv #(Math/abs %) coord))
    (apply +   (mapv #(Math/abs %) coord))))

(defn path-to-coord [path]
  (reduce #(mapv + %1 (directions %2)) [0 0] path))

;; tests
#_(and
   (= 3 (distance (path-to-coord '(ne ne ne))))
   (= 0 (distance (path-to-coord '(ne ne sw sw))))
   (= 2 (distance (path-to-coord '(ne ne s s))))
   (= 3 (distance (path-to-coord '(se,sw,se,sw,sw)))))

; part 1
#_(distance (path-to-coord data))
;; => 812

(defn paths-to-coord [path]
  (reductions #(mapv + %1 (directions %2)) [0 0] path))

;; part 2
#_(reduce max (map distance (paths-to-coord data)))
