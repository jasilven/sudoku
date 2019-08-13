(ns animation
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [sudoku :as sudoku]))

(def size 600)
(def ch-size (/ size 9))
(def x-delta (/ ch-size 7.5))
(def y-delta (* 0.85 ch-size))
(def border1 (/ size 3))
(def border2 (* 2 border1))

(defn new-state []
  (let [sudoku (rand-nth sudoku/sudokus)
        blank (sudoku/next-blank sudoku)]
    {:initial sudoku
     :state (list {:nums sudoku
                   :pos blank
                   :cands (sudoku/candidates sudoku blank)})}))
(defn setup []
  (q/frame-rate 15)
  (q/background 240)
  (q/color-mode :rgb)
  (q/text-font (q/create-font "Courier New" ch-size))
  (new-state))

(defn update-state [state]
  (if (:result state)
    (do (q/delay-frame 3000)
        (new-state))
    (let [new-state (sudoku/solve-iteration (:state state))
          result (:result new-state)]
      (if (nil? result)
        (assoc state :state new-state)
        (assoc state :result result)))))

(defn draw-borders []
  (q/stroke 250 250 250)
  (q/line border1 0 border1 size)
  (q/line border2 0 border2 size)
  (q/line 0 border1 size border1)
  (q/line 0 border2 size border2))

(defn draw-state [state]
  (let [result (:result state)
        nums (if (nil? result) (:nums (first (:state state))) result)]
    (q/clear)
    (draw-borders)
    (doseq [y (range 0 9)
            x (range 0 9)]
      (let [pos (+ x (* y 9))
            num (nth nums pos)]
        (when-not (zero? num)
          (if (zero? (nth (:initial state) pos))
            (q/fill 230 0 0)
            (q/fill 250 250 250))
          (q/text (str num)
                  (+ x-delta (* ch-size x))
                  (+ y-delta (* ch-size y))))))))

(defn -main []
  (q/defsketch sudoku-sketch
    :host "host"
    :title "Sudoku Solver"
    :size [size size]
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))
