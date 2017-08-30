(ns perlin.vector
  (:require [quil.core :as q]))

(defn from-angle
  ([ang]
   [(Math/cos ang)
    (Math/sin ang)])
  ([ang limit]
   (map #(* limit %)
        (from-angle ang))))

(defn random
  ([]
   (from-angle (rand Math/PI)))
  ([x]
   (from-angle (rand Math/PI) x))
  ([x y]
   [(rand x)
    (rand y)]))

(defn make-vector [x y]
  [x y])

(defn draw [vector]
  (let [[x y] vector]
    (q/point x y)))

(defn add [v1 v2]
  (let [[x1 y1] v1
        [x2 y2] v2]
    [(+ x1 x2)
     (+ y1 y2)]))

(defn perlin-vector
  ([x y z]
   (perlin-vector x y z q/noise))
  ([x y z perlin]
   (let [angle (* 2 2 Math/PI
                  (perlin x y z))]
     (from-angle angle 6))))

(defn angle
  ([x y]
   (let [a (Math/atan2 y x)]
     (if (< a 0)
       (+ a (* Math/PI 2))
       a)))
  ([[x y]]
   (angle x y)))

(defn magnitude [v]
  (let [[x y] v]
    (Math/sqrt (+ (* x x)
                  (* y y)))))

(defn draw-perlin [v x y scl]
  (let [[xv yv] v
        a (angle xv yv)]
    (q/with-translation [(* x scl) (* y scl)]
      (q/with-rotation [a]
        (q/stroke 0)
        (q/stroke-weight 1)
        (q/line 0 0 scl 0)))))

(defn limit [v speed]
  (if (= speed -1)
    v
    (if (> (magnitude v) speed)
      (from-angle (angle v) speed)
      v)))

(defn sq [x]
  (* x x))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (sq (- x2 x1))
                (sq (- y2 y1)))))
