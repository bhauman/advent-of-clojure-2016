(ns advent-of-clojure-2016.day11
  (:require
   [advent-of-clojure-2016.utils :as u]
   [clojure.math.combinatorics :as combo]
   [medley.core :refer [distinct-by]]
   [clojure.string :as string]
   [clojure.set :refer [difference union intersection]]))

#_(remove-ns 'advent-of-clojure-2016.day11)

(def start-state {:pos 0
                  :floors {0 #{1 -1}
                           1 #{10 100 1000 10000}
                           2 #{-10 -100 -1000 -10000}
                           3 #{}}})

(def valid-floors?
  (memoize
   (fn [n]
     (let [gens  (set (keep second n))
           chips (set (keep first (filter #(apply not= %) n)))]
       (empty? (intersection gens chips))))))

(def finished-score 10000000)

(defn finished? [n] (= #{[3 3]} (set n)))

(def score
  (memoize
   (fn [n]
     (if (finished? n)
       finished-score
       (* 10 (count (filter #(= 3 %) (flatten n))))))))

(def safe-elevator?
  (memoize
   (fn [positions]
     (or (= 1 (count positions))
         (every? odd? positions)
         (every? even? positions)
         (let [chip (some #(when (even? %) %) positions)]
           ((set positions) (inc chip)))))))

(def next-possible-states
  (memoize
   (fn [[flr pairings]]
     (let [flattened   (vec (flatten pairings))
           lower-bound (reduce min flattened)
           places      (u/indexes-by #(= % flr) flattened)
           positions'  (->> (map list places)
                            (concat (combo/combinations places 2))
                            (filter safe-elevator?))
           moves  (for [positions  positions'
                        up-or-down [1 -1]
                        :let  [next-floor (+ flr up-or-down)]
                        :when (<= lower-bound next-floor 3)]
                    [next-floor
                     (->> (reduce #(assoc %1 %2 next-floor) flattened positions)
                          (partition 2)
                          (map vec)
                          sort)])]
       (doall
        (sequence
         (comp
          (filter (comp valid-floors? second))
          (distinct-by second)
          (map #(vary-meta % assoc :score (score (second %)))))
         moves))))))

;; helpers to be able to look at and reason about the optimized state
(defn to-normal* [n]
  (reduce (fn [accum [i [c g]]]
            (-> accum
                (update-in [c] conj (int (- (Math/pow 10 i))))
                (update-in [g] conj (int (Math/pow 10 i)))))
          (into (sorted-map) (zipmap (range 4) (repeat #{})))
          (map-indexed vector n)))

(defn to-normal [[flr pairings]]
  {:pos flr
   :floors (to-normal* pairings)})

(defn canonical [floors]
  (let [kys  (set (map #(java.lang.Math/abs %) (apply concat (vals floors))))
        init (zipmap kys (repeat [nil nil]))]
    (-> (reduce (fn [accum [p items]]
                  (reduce #(assoc-in %1 [(java.lang.Math/abs %2)
                                         (if (pos? %2) 1 0)] p) accum items))
                init floors)
        vals
        sort)))

(defn to-canonical [{:keys [pos floors]}]
  [pos (canonical floors)])


(def state-score #(-> % second meta :score))

;; do one level at a time
(defn breadth-first-level [ordered-state-set]
  (println "level" (count (ffirst ordered-state-set)))
  (println "count" (count ordered-state-set))
  (->> ordered-state-set
       (mapcat (fn [[prev-states state]]
                 (let [last-canonical (second (last prev-states))]
                   (->> (next-possible-states state)
                        (filter #(not= last-canonical (second %)))
                        #_(filter (comp (complement (set prev-states)) second))
                        (map #(vector (conj prev-states state) %))))))
       (distinct-by second)
       (sort-by state-score >)
       (take 500)))

(defn breadth-first-search [limit start-state]
  (->> (iterate breadth-first-level [[[] (to-canonical start-state)]])
       (take-while #(and (not-empty %)
                         (let [scr (state-score (first %))]
                           (println "-" scr)
                           (not= scr finished-score))))
       (take limit)
       count))

;; part 1
#_(time (breadth-first-search 40 start-state))
;; => 33

(def start-state2 {:pos 0
                   :floors {0 #{1 -1 100000 -100000 1000000 -1000000}
                           1 #{10 100 1000 10000}
                           2 #{-10 -100 -1000 -10000}
                           3 #{}}})

;; part 2
#_(time (breadth-first-search 60 start-state2))
;; => 57

