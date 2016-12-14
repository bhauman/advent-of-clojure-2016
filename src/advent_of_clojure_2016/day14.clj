(ns advent-of-clojure-2016.day14
  (:require
   [digest :refer [md5]]))

(defn next-hash [salt i] (md5 (str salt i)))

(defn stretched-hash [salt i]
  (loop [c 0 h (next-hash salt i)]
    (if (= c 2016)
      h
      (recur (inc c) (md5 h)))))

#_(time (stretched-hash "abc" 0))

(defn triple-and-fives [h]
  (let [trip  (second (re-find #"(.)\1\1" h))
        fives (set (map second (re-seq #"(.)\1\1\1\1" h)))]
    (not-empty
     (cond-> {}
       trip (assoc :triple trip)
       (not-empty fives) (assoc :fives fives)))))

(defn code-key? [hasher salt i]
  (if-let [trip (get (hasher salt i) :triple)]
    (->> (range (inc i) (+ 1000 i))
         (map (partial hasher salt))
         (some #(when-let [f-set (:fives %)]
                  (when (f-set trip) i))))))

(defn find-answer [hasher salt]
  (let [hash-data-fn (memoize (comp triple-and-fives hasher))]
    (time (->> (keep #(code-key? hash-data-fn salt %) (range))
               (take 64)
               (map #(do (prn %) %))
               last))))

;; part 1
#_(find-answer next-hash "ahsbgdzn")
;; => 23890

;; part 2
#_(find-answer stretched-hash "ahsbgdzn")
;; => 22696

(comment
  (code-key? (comp triple-and-fives next-hash) "abc" 39)
  (code-key? (comp triple-and-fives next-hash) "abc" 22728)
  (find-answer next-hash "abc")

  (code-key? (comp triple-and-fives stretched-hash) "abc" 5)
  (code-key? (comp triple-and-fives stretched-hash) "abc" 10)
  (find-answer stretched-hash "abc")
  )
