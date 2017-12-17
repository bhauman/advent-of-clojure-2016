(ns advent-of-clojure-2017.day17)

(def input-steps 335)

;; Use a circular linked list that takes the form
;; {a -> b, b -> c, c -> a}
;; this allows a fast insertion of DD at b like so
;; {a -> b, b -> DD, DD -> c,  c -> a}

;; using an Array of Long for the underlying associative structure
;; for speed
(defn splice-in-fast [^long step ^longs state ^long current-number]
  (loop [previous-number (dec current-number)
         step-count (mod step current-number)]
    (if (zero? ^long step-count)
      (let [next' (aget state previous-number)]
        (doto state
          (aset previous-number current-number)
          (aset current-number next')))
      (recur (aget state previous-number) (dec ^long step-count)))))

(def start-state (comp long-array inc))

#_(set! *warn-on-reflection* true)
#_(set! *unchecked-math* :warn-on-boxed)

;; part 1
#_ (-> (reduce (partial splice-in-fast input-steps) (start-state 2017)
               (range 1 2018))
       (get 2017)
       time)
;; Elapsed time: 6.914165 msecs
;; = 1282

;; part 2
#_ (-> (reduce (partial splice-in-fast input-steps) (start-state 50000000)
               (range 1 50000001))
       (get 0)
       time)
;; => 27650600
;; around 5 minutes to complete


;; idea from @val_waeselynck

;; better solution for part 2 is to just find the last change to the 1 position
;; this only works for the 1 position in the puzzle, this is a bit subtle

(defn transition [^long position ^long v]
  (inc ^long (mod (+ position ^long input-steps) v)))

(defn find-last-after-zero [cycles]
  (loop [n 1
         pos 0
         after-zero nil]
    (let [next-pos (transition pos n)]
      (if (= n cycles)
        after-zero
        (recur (inc n)
               ^long next-pos
               (if (= 1 ^long next-pos) n after-zero))))))

;; better part 2
#_(time (find-last-after-zero (inc 50000000)))
;; => 27650600
;; Elapsed time: 4738.406857 msecs
