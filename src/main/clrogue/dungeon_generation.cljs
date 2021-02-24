(ns clrogue.dungeon-generation
  (:require [clrogue.graph-search :as graph-search])
  (:require [clrogue.tilemap :as tilemap]))

;; TODO this needs to be centered.
(defn rectangle-center [[x y w h]]
  [(Math/round (+ x (/ w 2))) (Math/round (+ y (/ h 2)))])
(defn any-overlap? [[x1 y1 w1 h1] [x2 y2 w2 h2]]
  (and (<= x1 (+ x2 w2)) (<= x2 (+ x1 w1))
       (<= y1 (+ y2 h2)) (<= y2 (+ y1 h1))))
(defn point-in-rectangle? [[rx ry rw rh] [x y]]
  (and (>= x rx) (<= x (+ rx rw))
       (>= y ry) (<= y (+ ry rh))))
(defn contained? [bounds-rectangle [x1 y1 w1 h1]]
  (and (point-in-rectangle? bounds-rectangle [x1 y1])
       (point-in-rectangle? bounds-rectangle [(+ x1 w1) y1])
       (point-in-rectangle? bounds-rectangle [x1 (+ y1 h1)])
       (point-in-rectangle? bounds-rectangle [(+ x1 w1) (+ y1 h1)])))

(defn any-overlaps? [rooms room] (some #(any-overlap? % room) rooms))
(defn walkable-area [[room-x room-y room-w room-h]]
  [(inc room-x)
   (inc room-y)
   (- room-w 2)
   (- room-h 2)])

(defn random-room [] [(rand-int 80) (rand-int 24) (+ (rand-int 5) 5) (+ (rand-int 5) 5)])
(defn non-intersecting-random-rooms [suggested-room-count max-attempts bounds-rect]
  (letfn [(try-to-place-room [rooms]
            (loop [rooms rooms
                   attempt-counter 0]
              (if (> attempt-counter max-attempts)
                rooms
                (let [room (random-room)]
                  (if (or (any-overlaps? rooms room)
                          (if bounds-rect (not (contained? (-> bounds-rect
                                                               (update 2 dec)
                                                               (update 3 dec))
                                                           room))))
                    (recur rooms (inc attempt-counter))
                    (conj rooms room))))))]
    (loop [rooms []
           placed-or-skipped-rooms 0]
      (if (= placed-or-skipped-rooms suggested-room-count)
        rooms
        (recur (try-to-place-room rooms)
               (inc placed-or-skipped-rooms))))))
(defn tunneling [suggested-room-count dungeon-bounds]
  (let [rooms (non-intersecting-random-rooms suggested-room-count 10 dungeon-bounds)
        edges (filter #(= (count %) 2) 
                      (loop [edges [] indices (range (count rooms))]
                        (if (empty? indices)
                          edges
                          (recur (into edges [(take 2 indices)])
                                 (drop 1 indices)))))]
    [rooms edges dungeon-bounds]))

(defn map-index-2d [function collection]
  (map-indexed
   (fn [row-index row]
     (map-indexed
      (fn [col-index col]
        (function [row-index col-index] col))
      row))
   collection))

(defn map-index-2dv [function collection]
  (vec (map-indexed
        (fn [row-index row]
          (vec (map-indexed
                (fn [col-index col]
                  (function [row-index col-index] col))
                row)))
        collection)))

(defn paint-room [tilemap room]
  (map-index-2dv (fn [[y1 x1] tile]
                   (if (point-in-rectangle? room [x1 y1])
                     (if (point-in-rectangle? (walkable-area room) [x1 y1])
                       \. \#)
                     tile)) tilemap))
;; (defn paint-line [tilemap [x1 y1] [x2 y2]]
;;   )
(defn paint-corridor [tilemap [rectangle-a rectangle-b]]
  (let [center-a (rectangle-center rectangle-a)
        center-b (rectangle-center rectangle-b)
        path-to-center (graph-search/breadth-first-search
                        tilemap center-a center-b
                        (fn [graph [x y]]
                          (filter (fn [point]
                                    ;; TODO note
                                    ;; if I'm around the dungeon
                                    ;; check if someone already carved a wall where I'm going
                                    ;; to carve. (I just don't want two block openings)
                                    (and (tilemap/in-bounds? graph point)
                                         (or (and (= (tilemap/at tilemap point) \#)
                                                  (or (point-in-rectangle? rectangle-a point)
                                                      (point-in-rectangle? rectangle-b point)))
                                             (not (= (tilemap/at tilemap point) \#)))))
                                  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))]
    (reduce (fn [tilemap [x y]]
              (assoc-in tilemap [y x] \.))
            tilemap
            path-to-center)))
(defn paint-rooms-and-edges [[rooms edges [bx by bw bh]]]
  (as-> (tilemap/make bw bh \space) tilemap
    (reduce paint-room tilemap rooms)
    (reduce #(paint-corridor %1 [(get rooms (first %2))
                                 (get rooms (second %2))]) tilemap edges)))
