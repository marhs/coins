(ns coins.core
  (require [incanter.core :as i]
           [incanter.charts :as c]))

(defn dice []
  (+ 1.001 (rand-int 6)))

(defn next [steps]
  "Given a list of throws, reset or throw again"
  (let [last-throw (last steps)
        next-throw (+ last-throw (dice))]
    (if (> next-throw 100)
      (conj steps 0)
      (conj steps next-throw))))

(defn simulate [times]
  "Throw the dice in the game n times"
  (loop [current 0
         steps [0]]
    (if (> times current)
      (recur (inc current) (next steps))
      steps)))

(defn sum-prob [freq values]
  (reduce + (map #(get freq %1) values)))

(defn score [list n]
  (float (sum-prob (frequencies (simulate n)) list)))

(i/view (c/histogram (simulate 10000) :nbins 100))
;(i/view (c/scatter-plot :a :b :data (frequencies (simulate 10000))))

;(score [4 5 6] 1000)
