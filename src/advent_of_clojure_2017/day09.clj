(ns advent-of-clojure-2017.day09
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (slurp (io/resource "2017/day09")))

(defn parse-structure [data]
  (-> data
      ;; eliminate escapes
      (string/replace #"(!+)(.)" #(if (odd? (count (second %))) "" (last %)))
      ;; replace garbage with garbage count
      (string/replace #"<[^>]*>" (comp str dec dec count))
      (string/replace "{" "(")
      (string/replace "}" ")")
      ;; parse as clojure data!!
      read-string))

(defn score
  ([data] (score data 1))
  ([data depth]
   (cond
     (number? data) 0
     (empty? data) depth
     :else (+ depth (apply + (map #(score % (inc depth)) data))))))

;; part 1
#_ (score (parse-structure data))
;;=> 13154

;; part 2
#_ (apply + (flatten (parse-structure data)))
;;=> 6369
