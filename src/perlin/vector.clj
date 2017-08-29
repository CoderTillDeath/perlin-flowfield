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
   (from-angle (rand 360)))
  ([x]
   (from-angle (rand 360) x))
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
