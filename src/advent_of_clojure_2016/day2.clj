(ns advent-of-clojure-2016.day2
  (:require
   [clojure.java.io :as io]))

(def lines (line-seq (io/reader (io/resource "day2"))))

(def dir {\U [0 -1]
          \D [0 1]
          \R [1 0]
          \L [-1 0]})

(def pos-number #(+ 5 (* 3 (second %)) (first %)))

(defn in-bounds? [[x y]]
  (and #(<= -1 x 1) #(<= -1 y 1)))

(defn code-to-pos [bound-fn start code]
  (->> code
       (map dir)
       (reduce (fn [a v]
                 (let [cp (map + a v)]
                   (if (bound-fn cp) cp a)))
               start)))

;; part 1
#_(->> lines
       (reductions (partial code-to-pos in-bounds?)
                   [0 0])
       rest
       (map pos-number))
;; => (7 8 2 9 3)

(defn in-part-2-bounds [[x y]]
  (and (<= -2 x 2)
       (let [bound (- 2 (Math/abs x))]
         (<= (- bound) y bound))))

(def keypad-map
  (into {}
        (map
         vector
         (->> (for [y (range -2 3)
                    x (range -2 3)]
                [x y])
              (filter in-part-2-bounds))
         (map inc (range)))))

;; part 2
#_(->> lines 
       (reductions (partial code-to-pos in-part-2-bounds) [-2 0])
       rest
       (map keypad-map)
       (map #(format "%X" %)))

; => ("A" "C" "8" "C" "8")
