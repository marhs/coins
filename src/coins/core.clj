(ns coins.core
  (require clojure.string))

;(i/view (c/histogram (simulate 10000) :nbins 100))
;(i/view (c/scatter-plot :a :b :data (frequencies (simulate 10000))))

;(score [4 5 6] 1000)

;;;;;;;;;;;;;;;;
; Clojure - Go ;
;;;;;;;;;;;;;;;;

; Una casilla es 
;   0 1 2 3
;   -------|
;0 | |o| | |
;  |-------|
;1 | | |x|x|
;  |-------|
;2 | |x|o|x|
;  |-------|
;3 |x|o|o| |
;   -------|

(defn generate-board [size]
  (into
   (reduce
    (fn [board elem]
      (into board {elem {:color       nil
                         :connections []
                         :liberties   []}}))
    {}
    (range (* size size)))
    {:size size}))


(def bboard (generate-board 19))


; Print board

(defn render-position [position]
  (if (get position :color)
    (if (= (get position :color)
          :white)
      "x"
      "o")
      " "))

(defn print-row [board row-num size]
  (str "|"
       (clojure.string/join "|"
                            (map #(render-position (get board %1))
                                 (range (* size row-num) (* size (+ 1 row-num)))))
       "|"))


(defn board->str [board]
  (let [size (get board :size)]
    (clojure.string/join "\n"
                         (map #(print-row board %1 size)
                              (range size)))))

(defn print-board [board]
  (println (board->str board))
  board)


(defn place-stone [board place color]
  (let [data (get board place)]
    (assoc-in board [place :color] color)))

(defn abs->coord [x size]
  (list (mod x size) (int (/ x size))))

(defn coord->abs [x y size]
  (+ (* size x) y))

(defn in-bounds? [x size]
  (and (>= x 0)
       (< x size)))

(defn neighbours [board pos]
  "Given a board and a position, return their adjacent positions"
  (let [size    (get board :size)
        coords  (abs->coord pos size)
        x       (first coords)
        y       (last coords)]
    (map (fn [[x y]] (coord->abs x y size))
      (filter (fn [[i j]] (and (in-bounds? i size)
                              (in-bounds? j size)))
              (list [(+ x 1) y]
                    [(- x 1) y]
                    [x (+ y 1)]
                    [x (- y 1)])))
    ))


(defn get-connections [board pos]
  (map (fn [x]
        (get-in board [x :connections]))
       (neighbours board pos)))

(defn connect-stones [board current-pos]
  (let [connections (get-connections board current-pos)]
    (set (reduce into [] connections))))
(def used-board
  (let [board (generate-board 4)]
    (place-stone 
     (place-stone board 0 :white
      ) 1 :black)))
(print-board used-board)
(connect-stones bboard 2)
