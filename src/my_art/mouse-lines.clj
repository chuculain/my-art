(ns my_art.mouse-lines
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; mouse position and history of lines drawn.
  {:mouse-x 0
   :mouse-y 0
   :lines []})

(defn update-state [state]
  ; Update state with current mouse position.
  (let [mx (q/mouse-x)
        my (q/mouse-y)
        center-x (/ (q/width) 2)
        center-y (/ (q/height) 2)
        new-line [center-x center-y mx my]]
    (-> state
        (assoc :mouse-x mx :mouse-y my)
        (update :lines conj new-line))))

(defn draw-state [state]
  ; Clear background with white color.
  (q/background 255)
  ; Draw all stored lines.
  (doseq [[x1 y1 x2 y2] (:lines state)]
    (q/stroke (mod x2 255) (mod y2 255) 200)
    (q/stroke-weight 1)
    (q/line x1 y1 x2 y2)))

(q/defsketch mouse-lines
  :title "Mouse Lines"
  :size [600 600]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
