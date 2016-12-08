(ns advent-of-clojure-2016.day7
  (:require
   [clojure.java.io :as io]))

(def data (line-seq (io/reader (io/resource "day7"))))

(defn abba? [[a b c d :as x]]
  {:pre [(= 4 (count x))]}
  (and (not= a b) (= a d) (= b c)))

(defn separate-parts [s]
  (->> s
       (partition-by #{\[ \]})
       (reduce (fn [{:keys [k] :as st} v]
                 (condp = v
                     [\[] (assoc st :k :neg)
                     [\]] (assoc st :k :pos)
                     (update-in st [k] conj v)))
               {:k :pos})))

(defn contains-abba? [s]
  (some abba? (partition 4 1 s)))

(defn supports-tls? [s]
  (let [{:keys [pos neg]} (separate-parts s)]
    (and (some contains-abba? pos)
         (every? (complement contains-abba?) neg))))

;; part 1
#_(count (filter identity (map supports-tls? data)))
;; => 115

(defn ssl? [[a b c :as x]]
  {:pre [(= 3 (count x))]}
  (and (not= a b) (= a c)))

(defn ssl-inv? [[a b c :as x] [a1 b1 c1 :as y]]
  {:pre [(= 3 (count x)) (= 3 (count y)) (ssl? x)]}
  (and (= a b1) (= b a1 c1)))

(defn contains-pred? [f s]
  (first (filter f (partition 3 1 s))))

(defn supports-ssl? [s]
  (let [{:keys [pos neg]} (separate-parts s)]
    (some identity
     (for [ssl-x (mapcat #(filter ssl? (partition 3 1 %)) pos)]
       (some #(contains-pred? (partial ssl-inv? ssl-x) %) neg)))))

;; part 2
#_(count (filter identity (map supports-ssl? data)))
;; => 231

