(ns advent-of-clojure-2016.day19)

;; the idea here is we can represent the data structure as a range
;; with a beginning and an end and a step

(defn next-level [{:keys [begin end step] :as st}]
  (let [nstep (* 2 step)]
    (if (= 1 (mod (- (inc end) begin) nstep))
      (-> st
          (assoc :step nstep)
          (update-in [:begin] #(+ % nstep)))
      (-> st
          (assoc :step nstep)
          (update-in [:end]  #(- % step))))))

;; part 1
#_(first
   (filter #(= (:begin %) (:end %))
           (iterate next-level {:begin 1 :end 3004953 :step 1})))
;; => 1815603

;; For part 2

;; this problem can be done in the same manner as above but it's very tricky
;; The following is a brute force approach with an optimized data structure and algorithm

;; this can be considered an alternate approach to part one as well

(defn opposite-index [cnt i]
  (mod (+ i (int (/ cnt 2))) cnt))

(defn make-st [s i]
  (let [st     (into (sorted-set) s)
        op-i   (opposite-index (count st) i)]
    [st (drop op-i st)]))

(defn handle-empty [[st prog :as x]]
  (if (empty? prog) [st (seq st)] x))

(defn drop-st* [[st progress :as x]]
  [(disj st (first progress)) (drop 1 progress)])

(defn keep-1* [[st prog]] [st (drop 1 prog)])

(defn skip-completed [f]
  (fn [x] (if (= 1 (count (first x))) x (f x))))

(def drop-st (skip-completed (comp drop-st* handle-empty)))

(def keep-1 (comp keep-1* handle-empty))

(declare transition)

(defn transition* [[st progress :as x]]
  (if (odd? (count st))
    (transition (keep-1 (drop-st x)))
    (keep-1 (drop-st (drop-st x)))))

(def transition (skip-completed transition*))

(defn find-answer2 [s]
  (->> (iterate transition (make-st s 0))
       (filter #(= 1 (count (first %))))
       first))

#_(time (find-answer2 (range 1 (inc 3004953))))
;; => 1410630
