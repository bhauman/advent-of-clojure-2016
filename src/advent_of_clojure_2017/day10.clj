(ns advent-of-clojure-2017.day10
  (:require
   [clojure.string :as string]
   [clojure.core.reducers :as r]))

(def data [102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216])

;; optimized for speed
(defn inplace-reverse [x pos length]
  (let [idx (mapv #(mod % (count x))
                  (range pos (+ pos length)))
        vs (r/reduce #(cons (get x %2) %1) '() idx)]
    (persistent!
     (loop [[id & idx'] idx
            [v & vs']    vs
            x          (transient x)]
       (if id
         (recur idx' vs' (assoc! x id v))
         x)))))

(defn transition [[x pos skip] length]
  [(inplace-reverse x pos length)
   (+ pos length skip)
   (inc skip)])

(def hash-round #(r/reduce transition %2 %1))

;; part 1
#_ (->> (hash-round data [(vec (range 256)) 0 0])
        first
        (take 2)
        (apply *))
;; => 5577

(defn sparse-hash [data]
  (first
   (nth (iterate (partial hash-round (concat data [17, 31, 73, 47, 23]))
                 [(vec (range 256)) 0 0])
        64)))

(defn dense-hash [sparseh]
  (->> sparseh
       (partition 16)
       (map #(reduce bit-xor %))
       (map #(format "%02x" %))
       (apply str)))

(def part-2 (comp dense-hash sparse-hash))

;; part 2
#_ (time (part-2 (map int (string/join "," data))))
;; Elapsed time: 55.868713 msecs
;; => 44f4befb0f303c0bafd085f97741d51d

