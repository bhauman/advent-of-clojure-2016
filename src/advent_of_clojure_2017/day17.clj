(ns advent-of-clojure-2017.day17
  (:require
   [clojure.core.reducers :as r]))

(def input-steps 335)

(defn splice-in-fast [^long step ^longs state ^long v]
  (let [steps (mod step v)]
    (loop [n (dec v)
           step-count steps]
      (if (zero? ^long step-count)
        (let [next (aget state n)]
          (doto state
            (aset n v)
            (aset v next)))
        (recur (aget state n) (dec ^long step-count))))))

(def start-state (comp long-array inc))

#_(set! *warn-on-reflection* true)
#_(set! *unchecked-math* :warn-on-boxed)

;; part 1
#_ (-> (r/reduce (partial splice-in-fast input-steps) (start-state 2017)
                 (range 1 2018))
       (get 2017)
       time)
;; Elapsed time: 6.914165 msecs
;; = 1282

;; part 2
#_ (-> (r/reduce (partial splice-in-fast input-steps) (start-state 50000000)
                 (range 1 50000000))
       (get 0)
       time)
;; => 27650600

