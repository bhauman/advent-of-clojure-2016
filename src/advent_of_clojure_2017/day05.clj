(ns advent-of-clojure-2017.day05
  (:require
   [clojure.java.io :as io]
   [clojure.core.reducers :as r]))

(def test-data [0 3 0 1 -3])

(def data (->> (io/resource "2017/day05")
               io/reader
               line-seq
               (mapv read-string)))

(defn interpret [{:keys [position instructions] :as env}]
  (when-let [inst (get instructions position)]
    (-> env
        (update :position + inst)
        (update-in [:instructions position] inc))))

;; part 1
#_(->> (iterate interpret {:position 0 :instructions data})
       (take-while identity)
       rest
       count)

(defn interpret-2 [[position instructions]]
  (when-let [inst (get instructions position)]
    [(+ position inst)
     (assoc! instructions position (if (>= inst 3) (dec inst) (inc inst)))]))

;; part 2
#_(time
   (r/reduce
    (fn [a _] (inc a)) ;; faster than (constantly 1)
    0
    (eduction
     (take-while identity)
     (iterate interpret-2 [0 (transient data)]))))

;Elapsed time: 7478.206687 msecs
;=> 28178177


;; faster to unroll the loop
#_(time
   (loop [step 0
          position 0
          instructions (transient data)]
     (if-let [inst (get instructions position)]
       (recur (inc step)
              (+ position inst)
              (assoc! instructions position (if (>= inst 3) (dec inst) (inc inst))))
       step)))
;; 5 seconds

;; experimenting with perf improvements
#_(time
 (loop [step 0
        position 0
        instructions (transient data)]
   (if-let [inst ^Long (get instructions position)]
     (recur ^Long (inc step)
            ^Long (+ position inst)
            (assoc! instructions position ^Long (if (>= inst 3) (dec inst) (inc inst))))
     step)))

#_(time
 (let [instructions (into-array Integer/TYPE test-data)]
   (loop [step ^int 0 position 0]
     (when (zero? (rem step 100))
       (prn step))
     (if-let [inst ^int (get instructions position)]
       (do
         (aset instructions position ^int (if (>= inst 3) (dec inst) (inc inst)))
         (recur ^int (inc step)
                ^int (+ position inst)))
     step))))








