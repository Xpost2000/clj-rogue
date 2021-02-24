(ns clrogue.tilemap)

(defn make [width height initial-element]
  (doall (vec (repeat height (doall (vec (repeat width initial-element)))))))

(defn dimensions [tilemap]
  [(count (first tilemap)) (count tilemap)])

(defn in-bounds? [tilemap [x y]]
  (let [[width height] (dimensions tilemap)]
    (and (and (>= x 0) (< x width))
         (and (>= y 0) (< y height)))))

(defn at [tilemap point]
  (get-in tilemap (reverse point) nil))

(defn good-neighbor? [tilemap point]
  (and (in-bounds? tilemap point)
       (not (= (at tilemap point) \#))))
