(ns advent-of-clojure-2016.day21
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def instr-data
  (->> (slurp (io/resource "day21"))
       (string/split-lines)
       (map #(read-string (str "[" % "]")))))

;; operations
(defn swap-letter [s x y]   (replace {x y y x} s))

(defn swap-position [s x y] (swap-letter s (nth s x) (nth s y)))

(defn rotate-left [s n]     (take (count s) (drop n (cycle s))))

(defn rotate-right [s n]    (reverse (rotate-left (reverse s) n)))

(defn move-position [s x y]
  (let [l   (nth s x)
        res (remove #(= l %) s)]
    (concat (take y res) (cons l (drop y res)))))

(defn reverse-positions [s x y]
  (let [part-size (inc (- y x))]
    (concat (take x s)
            (reverse (take part-size (drop x s)))
            (drop (+ x part-size) s))))

(defn rotate-based [s l]
  (let [i (.indexOf s l)]
    (rotate-right s (+ (inc i) (if (>= i 4) 1 0)))))

;; parse args
(def get-numbers (partial filter integer?))

(defn get-letters [instr]
  (->> (map str instr)
       (filter #(= 1 (count %)))
       (map first)))

;; interpret
(defn scramble-apply-instr [s instr]
  (let [s (vec s)]
    (condp = (take 2 instr)
      '[swap position]     (apply swap-position     s (get-numbers instr))
      '[swap letter]       (apply swap-letter       s (get-letters instr))
      '[move position]     (apply move-position     s (get-numbers instr))
      '[rotate left]       (apply rotate-left       s (get-numbers instr))
      '[rotate right]      (apply rotate-right      s (get-numbers instr))
      '[rotate based]      (apply rotate-based      s (get-letters instr))
      '[reverse positions] (apply reverse-positions s (get-numbers instr)))))

(defn scramble [s instrs]
  (reduce scramble-apply-instr s instrs))

;; part 1
#_(apply str (scramble "abcdefgh" instr-data))
;; => gbhafcde

;; not invertable so search
(defn inv-rotate-based [s l]
  (->> (map #(rotate-left s %) (range (count s)))
       (filter #(= (rotate-based % l) s))
       first))

(defn un-scramble-apply-instr [s instr]
  (let [s (vec s)]
    (condp = (take 2 instr)
      '[swap position]     (apply swap-position     s (reverse (get-numbers instr)))
      '[swap letter]       (apply swap-letter       s (reverse (get-letters instr)))
      '[move position]     (apply move-position     s (reverse (get-numbers instr)))
      '[rotate left]       (apply rotate-right      s (get-numbers instr))
      '[rotate right]      (apply rotate-left       s (get-numbers instr))
      '[rotate based]      (apply inv-rotate-based  s (get-letters instr))
      '[reverse positions] (apply reverse-positions s (get-numbers instr)))))

(defn un-scramble [s instrs]
  (reduce un-scramble-apply-instr s (reverse instrs)))

;; part 2
#_(apply str (un-scramble "fbgdceah" instr-data))
;; => "bcfaegdh"
