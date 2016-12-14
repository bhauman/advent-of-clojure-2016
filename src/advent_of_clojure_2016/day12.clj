(ns advent-of-clojure-2016.day12
  (:require
   [advent-of-clojure-2016.utils :as u]
   [clojure.java.io :as io]
   [clojure.string :as string]))

#_(remove-ns 'advent-of-clojure-2016.day12)

(defn parse-instructions [s]
  (->> s
       (string/split-lines)
       (map #(str "[" % "]"))
       (map read-string)))

(def input (parse-instructions (slurp (io/resource "day12"))))

(defn reg-or-val [s reg-val] (if (symbol? reg-val) (get s reg-val) reg-val))

(defn update-reg [s reg f]
  (-> s (update reg f) (update :pos inc)))

(defmulti  trans (fn [_ b] (first b)))
(defmethod trans 'inc [s [_ reg]] (update-reg s reg inc))
(defmethod trans 'dec [s [_ reg]] (update-reg s reg dec))

(defmethod trans 'cpy [s [_ reg-val reg]]
  (-> s (assoc reg (reg-or-val s reg-val)) (update :pos inc)))

(defmethod trans 'jnz [s [_ reg-val steps]]
  (update s :pos +
          (if-not (zero? (reg-or-val s reg-val)) steps 1)))

(def start-state '{a 0 b 0 c 0 d 0 :pos 0})

(defn run-prog [start-state input]
  (let [input (vec input)
        inst-count (count input)]
    (take-while (comp not nil?)
          (iterate (fn [{:keys [pos] :as s}]
                     (when (and s (< pos inst-count))
                       (let [inst (nth input pos)]
                         (trans s inst))))
                   start-state))))

;; part 1
#_(last (run-prog start-state input))

;; part 2
#_(last (run-prog (assoc start-state 'c 1) input))

