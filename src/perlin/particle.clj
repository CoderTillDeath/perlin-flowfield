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
   (make-particle pos vel (v/make-vector 0 0)))
  ([pos vel acc]
   {:pos pos
    :vel vel
    :acc acc}))

(defn make-particle-speed [x y speed]
  (make-particle (v/make-vector x y)
                 speed))

(defn apply-force [particle force]
  (update-in particle [:acc] #(v/add % force)))
 
(defn follow [particle #_flowfield scl cols zoff incr]
  (let [[x y] (:pos particle)
        xoff (int (/ x scl))
        yoff (int (/ y scl))
        index (+ xoff (* yoff cols))
        force (v/perlin-vector (/ xoff incr) (/ yoff incr) zoff)]
    #_(v/draw-perlin force xoff yoff scl)
    (apply-force particle
                 force
                 #_(nth flowfield index))))

(defn move [particle speed]
  (let [{velocity :vel
         acceleration :acc}
        particle]
   (-> particle
       (update :pos #(v/add % velocity))
       (update :vel #(v/limit (v/add % acceleration) speed)) 
       (update :acc (constantly (v/make-vector 0 0))))))

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
    (update particle :pos (constantly (v/make-vector newX newY)))))

