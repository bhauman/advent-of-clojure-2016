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

(defn other-member [h? [h t]]
  (if (= h? h) t h))

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



;; A tree-seq based solution
;; it collects the needed totals as it travels

(defn branch? [[parts-index available-pin strength-total length]]
  (parts-index available-pin))

(defn children [[parts-index available-pin strength-total length]]
  (when-let [childs (parts-index available-pin)]
    (map
     (fn [next-part]
       [(remove-part parts-index next-part)
        (other-member available-pin next-part)
        (+ strength-total (apply + next-part))
        (inc length)])
     childs)))

;; part 1
#_(->> (tree-seq branch? children [index 0 0 0])
       (map #(nth % 2))
       (reduce max)
       time)
;; => 1906
;; Elapsed time: 4472.040553 msecs

;; part 2
#_(time
   (let [bridges (tree-seq branch? children [index 0 0 0])
         max-length (reduce max (map last bridges))]
     (->> bridges
          (filter #(= (last %) max-length))
          (map #(nth % 3))
          (reduce max))))
;; => 1824
;; Elapsed time: 6435.686022 msecs

