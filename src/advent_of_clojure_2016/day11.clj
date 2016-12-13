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

(def test-state
  {:pos 0
   :floors {0 #{-1 -10}
            1 #{1}
            2 #{10}
            3 #{}}})

(def valid-floors?
  (memoize
   (fn [n]
     (let [gens  (set (keep second n))
           chips (set (keep first (filter #(apply not= %) n)))]
       (empty? (intersection gens chips))))))

(def finished?
  (memoize
   (fn [n]
     (= #{[3 3]} (set n)))))

(def score
  (memoize
   (fn [n]
     (* 10 (count (filter #(= 3 %) (flatten n)))))))

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
           places      (map first (filter #(= (second %) flr)
                                          (map-indexed vector flattened)))
           positions'  (filter safe-elevator?
                               (concat 
                                (combo/combinations places 2)
                                (map list places)))
           moves  (doall
                   (for [positions positions'
                         mod-fn    [inc dec]
                         ;; lower bound is a major optimization
                         :when (<= lower-bound (mod-fn flr)  3)]
                     [(mod-fn flr)
                      (sort ;; sort to make it cannonical
                       (map vec
                            (partition 2 (reduce #(update-in %1 [%2] mod-fn)
                                                 flattened
                                                 positions))))]))]
       (->> moves
            (distinct-by second)
            (filter (comp valid-floors? second)))))))

#_(next-possible-states (to-canonical {:pos 3, :floors {0 #{-1}, 2 #{1}, 3 #{-10 10}}}))

#_(map to-normal
 (new-possible-moves [1 [[0 0] [2 1] [2 1] [2 1] [2 1]]
                     ]))
;; helpers to be able to look at the optimized state
(defn to-normal* [n]
  (reduce (fn [accum [i [c g]]]
            (-> accum
                (update-in [c] (fnil conj #{}) (int (- (Math/pow 10 i))))
                (update-in [g] (fnil conj #{}) (int (Math/pow 10 i))))
            ) (sorted-map) (map-indexed vector n)))

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

;; priority q
(def make-priority-q sorted-map)

(defn push-q [q pri v]
  (update-in q [pri] (fnil conj []) v))

(defn pop-q [priority-q]
  (let [[k [v & xs]] (first priority-q)]
    [v (if (empty? xs)
         (dissoc priority-q k)
         (assoc  priority-q k (vec xs)))]))

(defn search-help [{:keys [prior-q]}]
  (loop [priority-q prior-q]
    (when (not-empty priority-q)
      (let [[[prev-states state] priority-q] (pop-q priority-q)]
        (cond
          (finished? (second state))
          (do
            (clojure.pprint/pprint
             [(count prev-states)
              #_(map to-normal prev-states)
              (to-normal state)])
            {:result [(count prev-states)
                      (map to-normal prev-states)
                      (to-normal state)]
             :prior-q priority-q})
          :else
          (recur
           (let [last-canonical (second (last prev-states))]
             (->> (next-possible-states state)
                  ;; never look back
                  (filter #(not= last-canonical (second %)))
                  (reduce #(push-q %1
                                   (- (count prev-states)
                                      (score (second %2)))
                                   [(conj prev-states state) %2])
                          priority-q)))))))))

(defn search [state]
  (map :result
         (rest (iterate search-help
                 {:prior-q (push-q (make-priority-q) 0 [[] (to-canonical
                                                            state)])}))))

;; part 1
#_(->> (time (doall (take 40 (search start-state))))
       (map first)
       (reduce min))
;; => 33

(def start-state2 {:pos 0
                   :floors {0 #{1 -1 100000 -100000 1000000 -1000000}
                           1 #{10 100 1000 10000}
                           2 #{-10 -100 -1000 -10000}
                           3 #{}}})

;; part 2
#_(def res
  (->> (time (doall (take 40 (search start-state2))))
       (map first)
       #_(reduce min)))






(def score-sim
  (memoize
   (fn [n]
     (reduce + (map (fn [x] (int (Math/pow 10 (dec x))))
                    (filter identity (flatten [0 1 2 3]))))
     #_(map (fn [x] (Math/pow 10 x)) (flatten n))
     #_(* 10 (count (filter #(= 3 %) (flatten n)))))))


(defn next-states [[prev-states state]]
  (let [last-canonical (second (last prev-states))
        next-prev-state (conj prev-states state)
        prev-state-count (count prev-states)]
    (->> (next-possible-states state)
         ;; never look back
         (filter #(not= last-canonical (second %)))
         (map #(vector (- prev-state-count (score-sim (second %)))
                       [next-prev-state %])))))

(let [current-temp 15
      window-max   10
      target-temp 15]
  (max 1 (int
          (Math/ceil (* (- 1 (/ (min current-temp target-temp)
                                target-temp))
                        window-max)))))

(defn select-states [target-temp q]
  (let [current-temp (-> q first second first first count)
        window-max   10
        window       (max 1 (int
                             (Math/ceil (* (- 1 (/ (min current-temp target-temp)
                                                   target-temp))
                                           window-max))))
        kys (sort-by
             first
             (distinct-by first
                          (concat (take 2 q)
                                  #_(take 2 (shuffle (seq q))))))
        next-q (reduce #(dissoc %1 %2) q (map first kys))
        states (mapcat second kys)
        maxval (count states)
        ]
    [states next-q]
    (prn current-temp target-temp window)
    [states #_(take
      (min (int (rand 100000))
           (count states))
      (shuffle (filter
                #(>= target-temp (count (first %)))
                (reverse states))))
     #_(take (+ (* window window window window window
                 window
                 (/ window 10)
                 (/ window 10)
                 (/ window 10)
                 (/ window 10)
                 )
              (if (= window 1) 1 2000))
           (shuffle (filter
                     #(>= target-temp (count (first %)))
                     (reverse states)))
           #_(keep identity
                 (map-indexed (fn [i v]
                                (when (> (/ (inc i) maxval) (- (rand) 0.7))
                                  v))
                              states)))
     next-q]))

(count
 (first '[[[0 ([0 1] [0 2])]
           [1 ([0 2] [1 1])]
           [2 ([0 2] [2 2])]
           [3 ([0 3] [2 3])]
           [2 ([0 3] [2 2])]
           [3 ([0 3] [3 3])]]
          [2 ([0 2] [3 2])]]))

(-> pppppq first second first first count)

(select-states 30 pppppq)


(defn search-help-sim [{:keys [prior-q target]}]
  (loop [priority-q prior-q]
    (when (not-empty priority-q)
      (let [[current-states priority-q] (select-states target priority-q)
            
            #_(pop-q priority-q)]
        (prn "cur count" (count current-states))
        #_(clojure.pprint/pprint (to-normal (second current-state)))
        (let [next' (->> (apply concat (pmap  next-states current-states))
                         (group-by #(let [res (finished? (last (last (last %))))]
                                      res)))
              finished    (next' true)
              next-to-try (next' false)
              priority-q (reduce #(apply push-q %1 %2) priority-q next-to-try)]
          (if (not-empty finished)
            {:finished finished
             :prior-q priority-q}
            (recur priority-q)))))))


(def start-state-sim {:pos 0
                      :floors {0 #{1 -1 #_100000 #_-100000 }
                               1 #{10 100 1000 10000}
                               2 #{-10 -100 -1000 -10000}
                               3 #{}}})

(def test-state
  {:pos 0
   :floors {0 #{-1 -10}
            1 #{1}
            2 #{10}
            3 #{}}})

(defn search-sim [state]
  (map
   (comp count first second)
   (:finished
    (search-help-sim
     {:target 35
      :prior-q (push-q (make-priority-q) 0 [[] (to-canonical
                                                state)])}))))

(time (search-sim start-state-sim))





