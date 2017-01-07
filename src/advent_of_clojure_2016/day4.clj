(ns advent-of-clojure-2016.day4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def lines (line-seq (io/reader (io/resource "day4"))))

(defn parse-room [s]
  (let [parts (string/split s #"-")
        [id chk] (string/split (last parts) #"\[")]
    {:word  (apply concat (butlast parts))
     :chksum (butlast chk)
     :id (Integer/parseInt id)}))

(defn checksum [word]
  (->> word
       frequencies
       (sort-by (fn [[a b]] [(- b) (int a)]))
       (map first)
       (take 5)))

(defn real-room? [{:keys [word chksum] :as room}]
  (= (checksum word) chksum))

(comment
  (real-room? (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))
  (real-room? (parse-room "a-b-c-d-e-f-g-h-987[abcde]"))
  (real-room? (parse-room "not-a-real-room-404[oarel]"))
  (real-room? (parse-room "totally-real-room-200[decoy]"))
  )

;; part 1
#_(->> lines
       (map parse-room)
       (filter real-room?)
       (map :id)
       (reduce +))
;;=> 278221

(defn shift-letter [n letter]
  (-> letter
      int
      (- 97)
      (+ n)
      (mod 26)
      (+ 97)
      char))

(defn decrypt [{:keys [word id] :as room}]
  (assoc room :decrypted
         (apply str (map (partial shift-letter id) word))))

#_(decrypt (parse-room "qzmt-zixmtkozy-ivhz-343"))

;; part 2
#_(->> lines
       (map parse-room)
       (filter real-room?)
       (map decrypt)
       (filter #(re-matches #".*north.*" (:decrypted %)))
       first
       :id)
;; => 267


