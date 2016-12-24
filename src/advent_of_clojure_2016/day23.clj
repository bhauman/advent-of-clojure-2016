(ns advent-of-clojure-2016.day23
  (:require
   [advent-of-clojure-2016.utils :as u]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-instructions [s]
  (->> s
       (string/split-lines)
       (map #(str "[" % "]"))
       (map read-string)
       vec))

(def input (parse-instructions (slurp (io/resource "day23"))))

(defn reg-or-val [s reg-val] (if (symbol? reg-val) (get s reg-val) reg-val))

(defn update-reg [s reg f]
  (-> s (update reg f) (update :pos inc)))

(def reg? '#{a b c d})
(def reg-or-val? #(or (reg? %) (integer? %)))

(defmulti  trans (fn [_ b] (first b)))
(defmethod trans 'inc [s [_ reg]]
  (assert (reg? reg))
  (update-reg s reg inc))

(defmethod trans 'dec [s [_ reg]]
  (assert (reg? reg))
  (update-reg s reg dec))

(defmethod trans 'cpy [s [_ reg-val reg]]
  (assert (and (reg-or-val? reg-val) (reg? reg)))
  (-> s (assoc reg (reg-or-val s reg-val)) (update :pos inc)))

(defmethod trans 'jnz [s [_ reg-val steps]]
  (assert (and (reg-or-val? reg-val) (reg-or-val? steps)))
  (update s :pos +
          (if-not (zero? (reg-or-val s reg-val))
            (reg-or-val s steps)
            1)))

(defn tgl-inst [inst]
  (update-in inst [0]
             #(if (= 1 (count (rest inst)))
                (if (= 'inc %) 'dec 'inc)
                (if (= 'jnz %) 'cpy 'jnz))))

(defmethod trans 'tgl [{:keys [pos instr] :as s} [_ reg-val]]
  (assert (reg-or-val? reg-val))
  (let [pos-to-change (+ pos (reg-or-val s reg-val))]
    (if (get instr pos-to-change nil)
      (-> s
          (update-in [:instr pos-to-change] tgl-inst)
          (update :pos inc))
      (update s :pos inc))))

(defn start-state [instructions]
  (assoc '{a 0 b 0 c 0 d 0 :pos 0}
         :instr (vec instructions)))

;; for part 2

(defn multiply-instructions? [instructions]
  (= '[[inc a]
       [dec c]
       [jnz c -2]
       [dec d]
       [jnz d -5]]
     (take 5 instructions)))

(defn multiply [st]
  (-> st
      (update-in ['a] + (* (get st 'c) (get st 'd)))
      (update :pos + 5)))

(defn run-prog [start-state]
  (take-while (comp not nil?)
              (iterate (fn [{:keys [pos instr] :as s}]
                         (when-let [inst (get instr pos nil)]
                           (try
                             (if (multiply-instructions? (drop pos instr))
                               (multiply s)
                               (trans s inst))
                             (catch Throwable e
                               (println "Skipping " (pr-str inst))
                               (update s :pos inc)
                               #_(throw (ex-info "error" {:inst inst :st s}))))))
                       start-state)))

;; part 1
#_ (time (last (run-prog (assoc (start-state input) 'a 7))))
;; => 12654

;; part 2
#_ (time (last (run-prog (assoc (start-state input) 'a 12))))
;; => 479009214
