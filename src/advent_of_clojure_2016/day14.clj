(ns advent-of-clojure-2016.day14
  (:require
   [advent-of-clojure-2016.utils :refer [apply-times]]
   [digest :refer [md5]]
   [medley.core :refer [assoc-some]]))

(def next-hash (comp md5 str))

(def stretched-hash (comp (partial apply-times 2016 md5) next-hash))

(defn triple-and-fives [h]
  (-> {}
      (assoc-some :triple (second (re-find #"(.)\1\1" h)))
      (assoc-some :fives  (not-empty (set (map second (re-seq #"(.)\1\1\1\1" h)))))
      not-empty))

(defn code-key? [salted-hasher i]
  (when-let [{:keys [triple]} (salted-hasher i)]
    (->> (range (inc i) (+ 1000 i))
         (keep  (comp :fives salted-hasher))
         (some  #(contains? % triple)))))

(defn find-answer [index-has-key?]
  (time (->> (filter index-has-key? (range))
             (take 64)
             (map #(do (prn %) %))
             last)))

(defn make-index-pred [salted-hasher]
  (partial code-key? (memoize (comp triple-and-fives salted-hasher))))

;; part 1
#_(find-answer (make-index-pred (partial next-hash "ahsbgdzn")))
;; => 23890

;; part 2
#_(find-answer (make-index-pred (partial stretched-hash "ahsbgdzn")))
;; => 22696
