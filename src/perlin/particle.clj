(ns perlin.particle
  (:require [quil.core :as q]
            [perlin.vector :as v]))

(defn make-particle
  ([] {:pos (v/random (q/width) (q/height))
       :velocity (v/random)})
  ([x y xspeed yspeed]
   {:pos (v/make-vector x y)
    :velocity (v/make-vector xspeed yspeed)}))

(defn update [particle]
  (let [[x y] (:pos particle)
        [xspeed yspeed] (:velocity particle)]
    (make-particle (+ x xspeed)
                   (+ y yspeed)
                   xspeed
                   yspeed)))


