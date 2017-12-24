(ns advent-of-clojure-2017.day24
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data
  (->> (io/resource "2017/day24")
       slurp))

(defn make-index [raw-data]
  (->> (string/split-lines raw-data)
       (map #(string/replace % "/" " "))
       (map #(format "[%s]" %))
       (mapv read-string)
       (reduce (fn [accum [h t :as part]]
                 (-> accum
                     (update h (fnil conj #{}) part)
                     (update t (fnil conj #{}) part)))
               {})))

;; an indexed version of the data for constant time look ups 
(def index (make-index data))

(defn remove-part [parts-index [h t :as part]]
  (-> parts-index
      (update h disj part)
      (update t disj part)))

(defn strength [parts]
  (reduce + 0 (flatten parts)))

(defn other-member [head part]
  (assert ((set part) head))
  (or (first (filter #(not= head %) part))
      head))

;; ignores partial briges as they don't matter for solution
(defn all-bridges [parts-index main-part tail-part]
  (if (empty? (parts-index tail-part))
    [[main-part]]
    (->> (parts-index tail-part)
         (mapcat #(all-bridges (remove-part parts-index %) % (other-member tail-part %)))
         (mapv #(cons main-part %)))))

;; part 1
#_(with-redefs [all-bridges (memoize all-bridges)]
    (time (->> (all-bridges index [0 0] 0)
               (map strength)
               (reduce max))))
;; => 1906
;; Elapsed time: 21145.979859 msecs

;; part 2
#_(with-redefs [all-bridges (memoize all-bridges)]
    (time
     (let [bridges (all-bridges index [0 0] 0)
           max-length (reduce max (map count bridges))]
       (->> bridges
            (filter #(= (count %) max-length))
            (map strength)
            (reduce max)))))
;; => 1824
;; Elapsed time: 3697.887612 msec


