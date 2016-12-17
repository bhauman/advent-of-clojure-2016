(ns advent-of-clojure-2016.day17
  (:require
   [digest :refer [md5]]))

(def input "mmsxrhfx")

(def dir-map
  {[0 -1] \U [0 1] \D [-1 0] \L [1 0] \R })

(defn directions [code [path pos]]
  (->> (md5 (apply str code path))
       (take 4)
       (keep-indexed (fn [i v] (when (#{\b \c \d \e \f} v) i)))
       (map [[0 -1] [0 1] [-1 0] [1 0]])
       (map #(vector (conj path (dir-map %)) (map + pos %)))
       (filter #(let [[_ [a b]] %]
                  (and (<= 0 a 3) (<= 0 b 3))))))

(def finished? #(= [3 3] (second %)))

(defn breadth-first-level [input positions]
  (->> positions
       (filter (comp not finished?))
       (mapcat (partial directions input))
       (sort-by #(if (finished? %) 0 1))))

(defn find-answer [s]
  (->> (iterate (partial breadth-first-level s) [[[] [0 0]]])
       (take-while not-empty)
       (filter (comp finished? first))
       (map #(apply str (ffirst %)))))

;; part1
#_(first (find-answer input))
;; => RLDUDRDDRR

;; part 2
#_(count (last (find-answer input)))
;; => 590
