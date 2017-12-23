(ns advent-of-clojure-2017.day23
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def program
  (->> (io/resource "2017/day23")
       io/reader
       line-seq
       (map #(format "[%s]" %))
       (mapv read-string)))

;; make sure that there are no errant registers
(assert (= (set (filter symbol? (flatten (map rest program))))
           '#{a b c d e f g h}))

(defmulti instruct (fn [_ [cmd]] cmd))

(defn inc-program-counter [state]
  (update state :program-counter inc))

(defn reg-or-val [state Y]
  (if (number? Y) Y (get state Y 0)))

(defmethod instruct 'set [state [_ X Y]]
  (inc-program-counter
   (assoc state X (reg-or-val state Y))))

(defmethod instruct 'sub [state [_ X Y]]
  (inc-program-counter
   (update state X (fnil - 0) (reg-or-val state Y))))

(defmethod instruct 'mul [state [_ X Y]]
  (-> state
      (update X (fnil * 0) (reg-or-val state Y))
      (update :mul-counter (fnil inc 0))
      inc-program-counter))

(defmethod instruct 'jnz [state [_ X Y]]
  (if (not= ^long (reg-or-val state X) 0)
    (update state :program-counter + (reg-or-val state Y))
    (inc-program-counter state)))

(defn transition [{:keys [program-counter program] :as state}]
  (when-let [instruction (get program program-counter)]
    (assoc (instruct state instruction)
           :inst instruction)))

;; part 1
#_(->> (iterate transition {:program-counter 0
                            :program program})
       (take-while identity)
       (map :inst)
       (filter #(= 'mul (first %)))
       count
       time)
;; => 4225
;; Elapsed time: 74.587177 msecs


;; program analysis

(def program-2
  '[[set b 67]
    [set c b]
    [jnz a 2]
    [jnz 1 5]
    [mul b 100]
    [sub b -100000]
    [set c b]
    [sub c -17000]
    ;; outer loop
    [set f 1]
    [set d 2]
    ;; second loop over values for d from 2 => b
    [set e 2] 
    ;; inner loop over values for e from 2 => b
    [set g d]  
    [mul g e]
    [sub g b]  ;; g = (- (* d e) b)
    [jnz g 2]
    [set f 0]  ;; f = 0 when b is divisible by e
    [sub e -1] ;; inc e
    [set g e]
    [sub g b]  ;; g = (- e b)
    [jnz g -8] ;; jmp inner loop (exit when e == b)
    [sub d -1] ;; inc d
    [set g d]
    [sub g b]  ;; g = (- d b)
    [jnz g -13] ;; jmp second loop (exit when d == b)
    ;; constants
    ;; g = 0,
    ;; d = 2,
    ;; c = 123700
    ;; at end of second loop these are our values

    ;; variables
    ;; b = 106700 increases by 17 every outer loop
    ;; so outer loop has to run 1000 times

    ;; f is only 0 when b has an integer divisor
    ;; h increments when f is 0 
    [jnz f 2]
    [sub h -1] ;; inc h  
    [set g b]
    [sub g c] ;; (- b c)
    [jnz g 2] ;; true until (= b c) or until this runs 1000 times
    [jnz 1 3]
    [sub b -17] ;; b changes by + 17
    [jnz 1 -23]]) ;; jmp outer loop

;; Analysis Conclusion

;; this program counts numbers that are not prime over the range
;; from b to c stepping by 17

(defn not-prime? [n]
  (some #(= 0 (mod n %)) (range 2 n)))

;; part 2
(->> (filter not-prime? (range 106700 (inc 123700) 17))
     count
     time)
;; => 905
;; Elapsed time: 1752.15433 msecs
