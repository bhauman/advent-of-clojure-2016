(ns advent-of-clojure-2017.day09
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (slurp (io/resource "2017/day09")))

(defn remove-trash [data]
  (reduce
   (fn [{:keys [out trash] :as state} ch]
     (cond
       (and (empty? trash) (not= ch \<))
       (update state :out conj ch)
       (empty? trash) ;; and \< is implied here
       (update state :trash conj ch)
       (odd? (count (take-while #{\!} trash)))
       (update state :trash conj ch)              
       (= ch \>) (assoc state :trash '())
       (= ch \!) (update state :trash conj ch) ;; ADDED FOR PART 2
       :else
       (-> state
           (update :trash-count inc) ;; ADDED FOR PART 2
           (update :trash conj ch))))
   {:trash '()
    :trash-count 0 
    :out []}
   data))

(defn prep-data [data]
  (-> (apply str (:out (remove-trash data))) 
      (string/replace "," "")
      (string/replace "{" "(")
      (string/replace "}" ")")
      read-string))

(defn count-em
  ([data] (count-em data 1))
  ([data depth]
   (if (empty? data)
     depth
     (+ depth (apply + (map #(count-em % (inc depth)) data))))))

(def part-1 (comp count-em prep-data))

;; part 1
#_(part-1 data)
;;=> 13154

;; part 2
#_ (:trash-count (remove-trash data))
;;=> 6369







