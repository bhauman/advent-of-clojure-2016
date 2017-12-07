(ns advent-of-clojure-2017.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-data [lines]
  (->> lines
       (map #(str "[" % "]"))
       (map read-string)))

(def data (->> (io/resource "2017/day07")
               io/reader
               line-seq
               parse-data))

(defn index-data [graph-data]
  (reduce
   (fn [accum [name [weight] _ & children]]
     (-> (reduce #(assoc-in %1 [:parent %2] name) accum children)
         (assoc-in [:weights name] weight)
         (assoc-in [:children name] (set children))))
   {}
   graph-data))

(defn tree-root [parent-graph child]
  (last (take-while some? (iterate parent-graph child))) )

; part 1
#_(let [{:keys [parent]} (index-data data)]
    (tree-root parent (rand-nth (vals parent))))

;; the computational complexity of this problem doesn't merit
;; memoization of tree traversal
(defn node-total-weight [{:keys [weights children] :as index} node]
  (if-let [childs (not-empty (children node))]
    (apply + (weights node) (map (partial node-total-weight index) childs))
    (weights node)))

(defn different-child [children-and-weights]
  (let [grouped (group-by second children-and-weights)]
    (when (< 1 (count grouped))
      (let [[[[x _]]] (filter #(= 1 (count %)) (vals grouped))]
        x))))

(defn child-node-and-weight-totals [{:keys [children] :as index} node]
  (->> (children node)
       (map (juxt identity (partial node-total-weight index)))
       (into {})))

(defn find-deepest-unequal-child [{:keys [parent children] :as index}]
  (->> (tree-root parent (first (vals parent)))
       (iterate #(different-child (child-node-and-weight-totals index %)))
       (take-while some?)
       last))

(defn part-2 [data]
  (let [{:keys [parent children] :as index} (index-data data)
        child-node-weights    (partial child-node-and-weight-totals index)
        deepest-unequal-node  (find-deepest-unequal-child index)
        children-total-weight (apply + (vals (child-node-weights deepest-unequal-node)))
        target-weight (-> (child-node-weights (parent deepest-unequal-node)) ;; siblings
                          (dissoc deepest-unequal-node)
                          vals
                          first)]
    (- target-weight children-total-weight)))

#_ (time (part-2 data))
;;=> 1864


(comment
  (def test-data
    (-> "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"
      string/split-lines
      parse-data))
  )
