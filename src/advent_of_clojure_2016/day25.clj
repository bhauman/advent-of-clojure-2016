(ns advent-of-clojure-2016.day25
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-instructions [s]
  (->> s
       (string/split-lines)
       (map #(str "[" % "]"))
       (map read-string)
       vec))

(def input (parse-instructions (slurp (io/resource "day25"))))

(defn reg-or-val [s reg-val] (if (symbol? reg-val) (get s reg-val) reg-val))

(defn update-reg [s reg f]
  (-> s (update reg f) (update :pos inc)))

(defmulti  trans (fn [_ b] (first b)))

(defmethod trans 'inc [s [_ reg]]
  (update-reg s reg inc))

(defmethod trans 'dec [s [_ reg]]
  (update-reg s reg dec))

(defmethod trans 'out [s [_ reg-val]]
  (-> s
      (update :output conj (reg-or-val s reg-val))
      (update :pos inc)))

(defmethod trans 'cpy [s [_ reg-val reg]]
  (-> s (assoc reg (reg-or-val s reg-val)) (update :pos inc)))

(defmethod trans 'jnz [s [_ reg-val steps]]
  (update s :pos +
          (if-not (zero? (reg-or-val s reg-val))
            (reg-or-val s steps)
            1)))

(defn start-state [instructions]
  (assoc '{a 0 b 0 c 0 d 0 :pos 0}
         :instr (vec instructions)
         :output []))

;; for part 2

;; not general
(defn multiply-instructions? [instructions]
  (= '[[inc d]
       [dec b]
       [jnz b -2]
       [dec c]
       [jnz c -5]]
     (take 5 instructions)))

(defn multiply [st]
  (-> st
      (update 'd + (* (get st 'b) (get st 'c)))
      (assoc 'b 0)      
      (assoc 'c 0)
      (update :pos + 5)))

(defn run-prog [start-state]
  (take-while (comp not nil?)
              (iterate (fn [{:keys [pos instr] :as s}]
                         (when-let [inst (get instr pos)]
                           (if (multiply-instructions? (drop pos instr))
                             (multiply s)
                             (trans s inst))))
                       start-state)))

(defn try-start-int [n]
  (:output (last (take-while #(not= (count (:output %)) 9)
                    (run-prog (assoc (start-state input) 'a n))))))

;; part 1
#_(count
   (take-while #(not= [0 1 0 1 0 1 0 1] %)
               (map try-start-int (range 0 1000))))
;; => 198

