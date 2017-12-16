(ns advent-of-clojure-2017.day16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def dance-calls
  (mapv
   (fn [[cmd & args]]
     (cons cmd (map read-string (string/split (apply str args) #"\/"))))
   (string/split
    (slurp (io/resource "2017/day16"))
    #",")))

(def start-state '[a b c d e f g h i j k l m n o p])

(defmulti dance-mover (fn [_ [cmd]] cmd))

(defmethod dance-mover \s [state [_ n] ]
  (let [length (count state)]
    (vec (take length (drop (mod (- n) length) (cycle state))))))

(defmethod dance-mover \x [state [_ a b]]
  (assoc state
         a (get state b)
         b (get state a)))

(defmethod dance-mover \p [state [_ pa pb]]
  (let [a (.indexOf state pa)
        b (.indexOf state pb)]
    (dance-mover state [\x a b])))

(defn whole-dance [dance-calls state]
  (reduce dance-mover state dance-calls))

;; test data
#_(whole-dance '[[\s 1] [\x 3 4] [\p e b]] '[a b c d e] )

;; ANSWER part 1
#_(->> (whole-dance dance-calls start-state)
       (string/join ""))
;; => lbdiomkhgcjanefp


(def complete-dances (iterate (partial whole-dance dance-calls) start-state))

;; strategy find a period for which the pattern repeats itself
(defn find-cycle-period [whole-dance-seq]
  (->> (rest whole-dance-seq)
       (take-while #(not= (first whole-dance-seq) %))
       count
       inc))

#_ (find-cycle-period complete-dances)
;; => 56

;; the dance will cycle every 56 
#_ (= (nth complete-dances 56) start-state)
#_ (= (nth complete-dances 112) start-state)

;; part 2 answer
#_ (->> (find-cycle-period complete-dances)
        (mod 1000000000)
        (nth complete-dances)
        (string/join ""))
;; => ejkflpgnamhdcboi



