(ns advent-of-clojure-2017.day15)

(defn transition [f v]
  (rem (* ^long f ^long v) 2147483647))

(def a-transition (partial transition 16807))
(def b-transition (partial transition 48271))

(defn generate [a-start b-start]
  (rest
   (map =
        (map #(bit-and 2r1111111111111111 ^long %) 
             (iterate a-transition a-start))
        (map #(bit-and 2r1111111111111111 ^long %)
             (iterate b-transition b-start)))))

;; part 1
#_(time
   (count (filter identity
                  (take 40000000 (generate 783 325)))))
;; Elapsed time: 34519.967381 msecs
;; => 650

(defn generate-2 [a-start b-start]
  (rest
   (map =
        (map #(bit-and 2r1111111111111111 ^long %) 
             (filter #(zero? ^long (mod ^long % 4))
                     (iterate a-transition a-start)))
        (map #(bit-and 2r1111111111111111 ^long %)
             (filter #(zero? ^long (mod ^long % 8))
                     (iterate b-transition b-start))))))

#_(set! *unchecked-math* :warn-on-boxed)

;; part 2
#_(time
   (count (filter identity
                  (take 5000000 (generate-2 783 325)))))
;; => 336
;; Elapsed time: 17035.632906 msecs
