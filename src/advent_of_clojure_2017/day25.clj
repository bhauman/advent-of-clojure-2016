(ns advent-of-clojure-2017.day25
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-state-program [state-program]
  (mapv (comp
        read-string
        #(subs % 0 (dec (count %)))
        last
        #(string/split % #"\s"))
       state-program))

(def tape-program
  (->> (io/resource "2017/day25")
       io/reader
       line-seq
       (drop 2)
       (partition 10)
       (map rest)
       (map parse-state-program)))

(defn options->machine [machine [state current write direction next-state]]
  (assoc machine [state current] [write direction next-state]))

(defn program->state-machine [machine [state & args]]
  (reduce options->machine machine (map #(cons state %) (partition 4 args))))

(def state-machine (reduce program->state-machine {} tape-program))

(defn transition [{:keys [state-machine state position tape] :as turing}]
  (let [[write direction next-state] (get state-machine [state (get tape position 0)])]
    (-> turing
        (assoc-in [:tape position] write)
        (update :position (if (= 'right direction) (fnil inc 0) (fnil dec 0)))
        (assoc :state next-state))))

#_(->> (nth (iterate transition {:state-machine state-machine
                                 :state 'A
                                 :position 0
                                 :tape {}})
            12368930)
       :tape
       vals
       (filter #{1})
       count
       time)
;; => 2725
;;Elapsed time: 23475.201042 msecs
