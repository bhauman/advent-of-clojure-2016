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
  (assoc-in machine [state current] [write direction next-state]))

(defn program->state-machine [machine [state & args]]
  (reduce options->machine machine (map #(cons state %) (partition 4 args))))

(def state-machine (reduce program->state-machine {} tape-program))

(defn transition [state-machine [state ^long position tape]]
  (let [[write direction next-state] (get-in state-machine [state (get tape position 0)])]
    [next-state
     ((if (= 'right direction) inc dec) position)
     (assoc tape position write)]))

#_(->> (nth (iterate (partial transition state-machine) ['A 0 {}])
            12368930)
       last
       vals
       (reduce +)
       time)
;; => 2725
;; Elapsed time: 14319.600632 msecs
