(ns advent-of-clojure-2017.day05
  (:require
   [clojure.java.io :as io]
   [clojure.core.reducers :as r]))

(def test-data [0 3 0 1 -3])

(def data (->> (io/resource "2017/day05")
               io/reader
               line-seq
               (mapv read-string)
               (mapv int)))

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
;;=> 358309

(defn interpret-2 [[^int position instructions]]
  (when-let [inst ^int (get instructions position)]
    [(+ position inst)
     (assoc! instructions position (if (>= inst 3) (dec inst) (inc inst)))]))

;; part 2
#_(time
   (r/reduce
    (fn [^Long x _] (inc x)) ;; faster than (constantly 1)
    -1
    (eduction
     (take-while identity)
     (iterate interpret-2 [0 (transient data)]))))

;Elapsed time: 7478.206687 msecs
;=> 28178177

;; fastest with a native array and fully type hinted loop

(comment
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  )

#_(time
   (let [instructions ^ints (into-array Integer/TYPE data)]
     (loop [step 0 position 0]
       (if-let [inst (try (aget instructions position) (catch Throwable e nil))]
         (let [inst ^int inst]
           (aset instructions position
                 (if (>= inst 3) (dec inst) (inc inst)))
           (recur (inc step) (+ position inst)))
         step))))
;; "Elapsed time: 663.752469 msecs"








