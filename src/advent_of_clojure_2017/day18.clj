(ns advent-of-clojure-2017.day18
  (:require
   [clojure.java.io :as io]))

(def program
  (->> (io/resource "2017/day18")
       io/reader
       line-seq
       (map #(format "[%s]" %))
       (mapv read-string)))

(defmulti instruct (fn [_ [cmd]] cmd))

(defn inc-program-counter [state]
  (update state :program-counter inc))

(defn reg-or-val [state Y]
  (if (number? Y) Y (get state Y 0)))

(defmethod instruct 'snd [state [_ X]]
  (inc-program-counter
   (assoc state :snd (reg-or-val state X))))

(defmethod instruct 'set [state [_ X Y]]
  (inc-program-counter
   (assoc state X (reg-or-val state Y))))

(defmethod instruct 'add [state [_ X Y]]
  (inc-program-counter
   (update state X (fnil + 0) (reg-or-val state Y))))

(defmethod instruct 'mul [state [_ X Y]]
  (inc-program-counter
   (update state X (fnil * 0) (reg-or-val state Y))))

(defmethod instruct 'mod [state [_ X Y]]
  (inc-program-counter
   (update state X (fnil rem 0) (reg-or-val state Y))))

(defmethod instruct 'rcv [state [_ X]]
  (inc-program-counter
   (if (zero? ^long (reg-or-val state X))
     state
     (let [last-sound (get state :snd)]
       (assoc state
              X last-sound
              :rcv last-sound)))))

(defmethod instruct 'jgz [state [_ X Y]]
  (if (> ^long (reg-or-val state X) 0)
    (update state :program-counter + (reg-or-val state Y))
    (inc-program-counter state)))

(defn transition [{:keys [program-counter program] :as state}]
  (when-let [instruction (get program program-counter)]
    (instruct state instruction)))

;; part 1
#_(->> (iterate transition {:program-counter 0
                            :program program})
       (drop-while (complement :rcv))
       first
       :snd
       time)
;; => 8600
;; Elapsed time: 1.847176 msecs

;; override send and recieve
(defn instruct-part2 [state [cmd X :as inst]]
  (let [state (dissoc state :blocking)]
    (condp = cmd
      ;; send to output queue
      'snd (inc-program-counter
            (-> state
                (assoc :output-val (reg-or-val state X))
                ;; count the messages sent
                (update :output-count (fnil inc 0))))
      ;; recieve from input queue
      'rcv (let [{:keys [input-queue]} state]
             (if (empty? input-queue)
               (assoc state :blocking true)
               (inc-program-counter
                (assoc state
                       X (peek input-queue)
                       :input-queue (pop input-queue)))))
      ;; forward to part 1 interpreter
      (instruct state inst))))

(defn swap-communication-step [[state-a {:keys [output-val] :as state-b} :as states]]
  (if output-val
    [(update state-a :input-queue (fnil conj clojure.lang.PersistentQueue/EMPTY) output-val)
     (dissoc state-b :output-val)]
    states))

(def swap-communication
  (comp reverse swap-communication-step
        reverse swap-communication-step))

(defn exec-instruction [{:keys [program-counter program] :as state}]
  (if-let [instruction (get program program-counter)]
    (instruct-part2 state instruction)
    (assoc state :blocking true)))

(defn transition-2 [states]
  (mapv exec-instruction (swap-communication states)))

;; part 2
#_(->> (iterate transition-2
                [{:name 'A
                  'p 0
                  :program-counter 0
                  :program program}
                 {:name 'B
                  'p 1
                  :program-counter 0
                  :program program}])
       (drop-while #(not (every? :blocking %)))
       first
       second
       :output-count
       time)

;; not publishing until later as the inputs are similar
;; Elapsed time: 439.798548 msecs

#_(set! *unchecked-math* :warn-on-boxed)


