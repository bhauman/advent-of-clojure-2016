(ns advent-of-clojure-2017.day13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (->> (io/resource "2017/day13")
               io/reader
               line-seq
               (map #(string/replace % #":" ""))
               (map #(format "[%s]" %))
               (map read-string)
               (into (sorted-map))))

(defn scanner-position [count level-size]
  (let [level-size (dec level-size)
        division (/ count level-size)]
    (if (even? (long division))
      (rem count level-size)
      (- level-size (rem count level-size)))))

;; part 1
#_(->> data
       (map (juxt #(scanner-position (key %) (val %)) identity))
       (filter (comp zero? first))
       (map (comp #(apply * %) second))
       (apply +))
;; => 1476

(defn build-can-pass-at-time?-fn [data']
  (let [data-size (apply max (keys data'))]
    (fn [time-count]
      (some
       (fn [idx]
         (zero?
          (if-let [range' (data' idx)]
            (scanner-position (+ idx time-count) range')
            -1)))
       (range (inc data-size))))))

(defn how-many-pico-seconds-to-delay? [data']
  (->> (map (build-can-pass-at-time?-fn data') (range))
       (take-while identity)
       count))

;; part 2
#_ (time (how-many-pico-seconds-to-delay? data))
;; Elapsed time: 6779.875129 msecs
;; => 3937334


