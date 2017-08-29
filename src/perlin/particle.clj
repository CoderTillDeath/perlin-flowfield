(ns perlin.particle
  (:require [quil.core :as q]
            [perlin.vector :as v]))

(defn make-particle
  ([]
   (make-particle (v/random (q/width) (q/height))
                     (v/random 2)))
  ([x y xspeed yspeed]
   (make-particle (v/make-vector x y)
                  (v/make-vector xspeed yspeed)))
  ([pos vel]
   (make-particle pos vel (v/make-vector 0.1 0.1)))
  ([pos vel acc]
   {:pos pos
    :vel vel
    :acc acc}))

(defn make-particle-speed [x y speed]
  (make-particle (v/make-vector x y)
                 speed))

(defn apply-force [particle force]
  (make-particle (:pos particle)
                 (:vel particle)
                 (v/add (:acc particle)
                        force)))
 
(defn follow [particle flowfield scl cols]
  (let [[x y] (:pos particle)
        xoff (int (/ x scl))
        yoff (int (/ y scl))
        index (+ xoff (* yoff cols))]
    (apply-force particle (nth flowfield index))))

(defn update [particle speed]
  (let [{position :pos
         velocity :vel
         acceleration :acc}
        particle]
    (make-particle (v/add position velocity)
                   (v/limit (v/add velocity acceleration) speed))))

(defn draw [particle prev]
  (let [dist (v/distance (:pos prev) (:pos particle))
        [x1 y1] (:pos prev)
        [x2 y2] (:pos particle)]
    (q/stroke 1 10)
    (q/stroke-weight 1)
    (if (< dist 10)
      (q/line x1 y1 x2 y2)
      (q/point x2 y2))))

(defn only [x & rest]
  x)

(defn update-coordinate [x max]
  ((cond (< x 0) +
         (> x max) -
         :else only)
   x max))

(defn edges [particle]
  (let [[x y] (:pos particle)
        newX (update-coordinate x (q/width))
        newY (update-coordinate y (q/height))]
    (make-particle (v/make-vector newX newY)
                   (:vel particle))))

