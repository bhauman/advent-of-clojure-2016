(ns advent-of-clojure-2016.day6
  (:require
   [clojure.java.io :as io]))

(def data (line-seq (io/reader (io/resource "day6"))))

(def transpose (partial apply mapv vector))

(defn pull-by [sort-fn d]
  (->> d
       frequencies
       (sort-by sort-fn)
       ffirst))

;; part 1
#_(->> data
       transpose
       (map (partial pull-by (comp - second))))
;; => (\x \h \n \q \p \q \q \l)

;; part 2
#_(->> data
       transpose
       (map (partial pull-by second)))
;; => (\b \r \h \a \i \l \r \o)
