(ns perlin.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [perlin.particle :as p]
            [perlin.vector :as v]))
(def fancy (atom true))
(def incr (atom (/ 1 0.1)))
(def scl (atom 10))
(def speed (atom 5))

(defn perlin-vectors [rows cols zoff]
  (for [x (range 0 rows)
        y (range 0 cols)]
    (let [xoff (/ x @incr)
          yoff (/ y @incr)]
      (v/perlin-vector xoff yoff zoff))))

(defn setup []
  (q/background 255)
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (let [cols (quot (q/width) @scl)
        rows (quot (q/height) @scl)
        points (repeatedly 2000 p/make-particle)]
    {:cols cols
     :rows rows
     :zoff 0.0
     :points points
     :prev points
     ;:vectors (perlin-vectors rows cols 0.0)
     }))

(defn update-point [point #_vectors cols zoff]
  (-> point
      (p/follow #_vectors @scl cols zoff @incr)
      (p/move @speed)
      (p/edges)))

(defn old-update [point]
  (-> point
      (p/move @speed)
      (p/edges)))

(defn update-state [state]
  {:cols (quot (q/width) @scl)
   :rows (quot (q/height) @scl)
   :zoff (+ (:zoff state) 0.025)
   ;:vectors (perlin-vectors (:rows state) (:cols state) (:zoff state))
   :points (map #(update-point %
                               ;(:vectors state)
                               (:cols state)
                               (:zoff state))
                (:points state))
   #_(map old-update (:points state))
   :prev (:points state)
   })

(defn draw-particle [particle prev]
  (let [dist (v/distance (:pos prev) (:pos particle))
        [x1 y1] (:pos prev)
        [x2 y2] (:pos particle)]
    (if @fancy
      (q/stroke 1 5)
      (q/stroke 1))
    (q/stroke-weight 1)
    (if (< dist 100)
      (q/line x1 y1 x2 y2)
      (q/point x2 y2))))

(defn draw-vectors [state]
  (doseq [x (range 0 (:cols state))
        y (range 0 (:rows state))]
    ;(draw-lines x y state)
    (let [index (+ x (* y (:cols state)))]
      (v/draw-perlin (nth (:vectors state) index)
                     x
                     y
                     @scl))
    ))

(defn draw-field [state]
  (doseq [x (range 0 (:cols state))
          y (range 0 (:rows state))]
    (v/draw-perlin (v/perlin-vector (/ x @incr)
                                    (/ y @incr)
                                    (:zoff state))
                   x
                   y
                   @scl)))

(defn draw-state [state]
  (when (not @fancy)
    (q/background 255)
    (draw-field state))
  (doseq [[i j] (map vector (:points state) (:prev state))]
    (draw-particle i j)))
  

(q/defsketch perlin
  :title "You spin my circle right round"
  :size [600 600]
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
