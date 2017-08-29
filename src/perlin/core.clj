(ns perlin.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [perlin.particle :as p]
            [perlin.vector :as v]))

(def incr (atom (/ 1 0.1)))
(def scl (atom 10))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:cols (quot (q/width) @scl)
   :rows (quot (q/height) @scl)
   :zoff 0.0
   :points (repeatedly 10 p/make-particle)
   :vectors (repeatedly #([(rand) (rand)]))})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:cols (quot (q/width) @scl)
   :rows (quot (q/height) @scl)
   :zoff (+ (:zoff state) 0.025)
   :points (map p/update (:points state))
   :vectors (:vectors state)})

(defn draw-state [state]
  (q/background 255)
  #_(doseq [x (range 0 (:cols state))
        y (range 0 (:rows state))]
    (let [xoff (/ x @incr)
          yoff (/ y @incr)
          zoff (:zoff state)
          xtrans (* x @scl)
          ytrans (* y @scl)
          angle (* 2 Math/PI
                   (q/noise xoff yoff zoff))
          vect (v/from-angle angle)]
      ;(q/fill (* 255 (q/noise xoff yoff)))
      ;(q/rect (* x @scl) (* y @scl) @scl @scl)
      (q/stroke 0)
      (q/with-translation [xtrans ytrans]
        (q/with-rotation [angle]
          (q/line 0 0 @scl 0)))
      ;vect
      )
    )
  (doseq [i (:points state)]
    (p/draw i)))
  

(q/defsketch perlin
  :title "You spin my circle right round"
  :size [200 200]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
