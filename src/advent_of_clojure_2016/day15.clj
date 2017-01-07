(ns advent-of-clojure-2016.day15)

(def start-state [[5 17]
                  [8 19]
                  [1 7]
                  [7 13]
                  [1 5]
                  [0 3]])

(defn disc-at-t [[pos disc-size] t]
  [(mod (+ t pos) disc-size) disc-size])

(defn state-of-discs-at-t [s t]
  (map-indexed (fn [i disc] (disc-at-t disc (+ 1 t i))) s))

(def completed #(every? zero? (map first %)))

(defn times-with-aligned-discs [s start step]
  (->> (range start Integer/MAX_VALUE step)
       (filter #(completed (state-of-discs-at-t s %)))))

(defn next-step [{:keys [pos step]} state]
  (let [[next-pos after-pos]
        (take 2 (times-with-aligned-discs state pos step))]
    {:pos next-pos :step (- after-pos next-pos)}))

(defn find-answer [state]
  (reduce next-step {:pos 0 :step 1} (rest (reductions conj [] state))))

;; part 1
#_(find-answer start-state)
;; => 16824

#_(find-answer (conj start-state [0 11]))
;; => 3543984


