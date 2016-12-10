(ns advent-of-clojure-2016.day10
  (:require
   [advent-of-clojure-2016.utils :as u]
   [clojure.set :refer [difference]]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (string/split-lines (string/trim (slurp (io/resource "day10")))))

(defn addr-names [l]
  (->> (re-seq #"(bot|output)\s(\d+)" l)
       (map (fn [[_ a b]] (keyword (str a "_" b))))))

(defn parse-line [l]
  (let [addrs (addr-names l)]
    (condp = (first l)
      \v  (vector :to (first addrs) (u/to-int (first (re-seq #"\d+" l))))
      \b  (cons :from addrs))))

;; state shape
;; all commands originate at a unique bot
#_(let [froms (map second (filter #(= (first %) :from) input))]
    (= (count froms) (count (set froms))))

(defn give-value [accum [_ addr v]]
  (update-in accum [addr] (fnil conj #{}) v))

(defn move-value [accum from to v]
  {:pre [((get accum from) v)]} ;; must have val to give
  (-> accum 
      (update-in [from] disj v)
      (give-value [nil to v])))

(defn make-init [commands]
  {:commands (filter #(= (first %) :from) commands)
   :registers (reduce give-value {} (filter #(= (first %) :to) commands))})

(defn handle-bot-command [{:keys [registers] :as state} [_ from low-to high-to :as com]]
  (let [[lv hv]   ((juxt first last) (sort (get registers from #{})))]
    (assert (and lv hv (not= lv hv)))
    (update-in state [:registers]
               #(-> %
                    (move-value from low-to lv)
                    (move-value from high-to hv)))))

(defn active-registers [x] (->> x (filter #(>= (count (val %)) 2)) keys set))

(defn transition-state [{:keys [registers commands] :as state}]
  (let [active-regs                     (active-registers registers)
        [active-commands rest-commands] (u/pluck #(-> % second active-regs) commands)]
    (when-not (empty? active-commands)
      (-> (reduce handle-bot-command state active-commands)
          (assoc :commands rest-commands)))))

;; part 1
(->> (iterate transition-state (make-init (map parse-line data)))
     (take-while #(not (nil? %)))
     (map :registers)
     (keep #(some (fn [[k v]] (when (empty? (difference #{61 17} v)) k)) %))
     first)
;;=> :bot_161

;; part 2
(->> (iterate transition-state (make-init (map parse-line data)))
     (take-while #(not (nil? %)))
     last
     :registers
     (#(select-keys % [:output_0 :output_1 :output_2]))
     vals
     (map first)
     (apply *))
;; => 133163
