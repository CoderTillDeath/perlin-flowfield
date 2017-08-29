(ns perlin.vector)

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
