(ns perlin.particle
  (:require [quil.core :as q]
            [perlin.vector :as v]))

(defn make-particle
  ([] {:pos (v/random (q/width) (q/height))
       :vel (v/random)})
  ([x y xspeed yspeed]
   {:pos (v/make-vector x y)
    :vel (v/make-vector xspeed yspeed)})
  ([x y speed]
   (let [[xspeed yspeed] speed]
     (make-particle x y xspeed yspeed))))

(defn update [particle]
  (let [[x y] (:pos particle)
        [xspeed yspeed] (:vel particle)]
    (make-particle (+ x xspeed)
                   (+ y yspeed)
                   xspeed
                   yspeed)))

(defn draw [particle]
  (let [[x y] (:pos particle)]
    (q/stroke-weight 5)
    (q/point x y)))

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
        newY (update-coordinate (q/height))]
    (make-particle newX newY (:vel particle))))

