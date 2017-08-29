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
  ([x y speed]
   (make-particle (v/make-vector x y)
                  speed))
  ([pos vel]
   {:pos pos
    :vel vel
    :acc (v/make-vector 0 0)}))

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

(defn update [particle]
  (let [{position :pos
         velocity :vel}
        particle]
    (make-particle (v/add position velocity)
                  velocity)))

(defn limit-speed [particle]
    (+ 1 2))
    
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
        newY (update-coordinate y (q/height))]
    (make-particle newX newY (:vel particle))))

