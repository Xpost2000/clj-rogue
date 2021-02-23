(ns clrogue.game
  (:require [clrogue.canvas :as canvas])
  (:require [clrogue.input :as input])
  (:require [clrogue.graph-search :as graph-search]))
                                        ;
(defonce input-state (atom (input/new)))
(defonce game-canvas-context (.getContext (.getElementById js/document "game-canvas") "2d"))

(def white [255 255 255 255])
(def black [0 0 0 255])
(def blue [0 0 255 255])
(def green [0 255 0 255])
(def red [255 0 0 255])

(defn euclidean-distance [a b]
  (Math/sqrt (reduce + (map #(* % %) (map - b a)))))

(defn attenuation [constant distance]
  (/ 1.0 (+ 1 (* constant distance))))

;; TODO, make these take rest arguments
(defn vector-add [a b] (mapv + a b))
(defn vector-subtract [a b] (mapv - a b))
(defn vector-scale [a b] (mapv #(* % b) a))
(defn average [collection] (/ (reduce + 0 collection) (count collection)))
(defn vector-component-multiply [a b] (mapv * a b))

(defn contribution [base-color source to distance-function]
  (let [attenuation (attenuation 4.5 (distance-function to (:position source)))]
    (vector-scale
     (vector-component-multiply base-color (vector-scale (:color source) (:ambient-strength source)))
     (* (:power source) attenuation))))

(defn color-lighting [position base-color sources]
  (reduce vector-add
          (mapv #(contribution base-color % position euclidean-distance) sources)))

(defn game-coordinates->screen [[camera-x camera-y] [x y]]
  [(* (+ x camera-x) 8) (* (+ y camera-y) 16)])
(defn screen-coordinates->game [[camera-x camera-y] [x y]] :not-done)

(defn tilemap-dimensions [tilemap]
  [(count (first tilemap)) (count tilemap)])
(defn tilemap-in-bounds? [tilemap [x y]]
  (let [[width height] (tilemap-dimensions tilemap)]
    (and (and (>= x 0) (< x width))
         (and (>= y 0) (< y height)))))
(defn tilemap-get [tilemap point]
  (get-in tilemap (reverse point)))
(defn tilemap-good-neighbor? [tilemap point]
  (and (tilemap-in-bounds? tilemap point)
       (not (= (tilemap-get tilemap point) \#))))

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
(defn random-room [] [(rand-int 80) (rand-int 24) (+ (rand-int 5) 5) (+ (rand-int 5) 5)])

(defn non-intersecting-random-rooms [suggested-room-count max-attempts bounds-rect]
  (letfn [(try-to-place-room [rooms]
            (loop [rooms rooms
                   attempt-counter 0]
              (if (> attempt-counter max-attempts)
                rooms
                (let [room (random-room)]
                  (if (or (any-overlaps? rooms room)
                          (if bounds-rect (not (contained? bounds-rect room)) false))
                    (recur rooms (inc attempt-counter))
                    (conj rooms room))))))]
    (loop [rooms []
           placed-or-skipped-rooms 0]
      (if (= placed-or-skipped-rooms suggested-room-count)
        rooms
        (recur (try-to-place-room rooms)
               (inc placed-or-skipped-rooms))))))
(defn tunneling-dungeon-gen [suggested-room-count dungeon-bounds]
  (let [rooms (non-intersecting-random-rooms suggested-room-count 10 dungeon-bounds)
        edges (filter #(= (count %) 2) 
                      (loop [edges [] indices (range (count rooms))]
                        (if (empty? indices)
                          edges
                          (recur (into edges [(take 2 indices)])
                                 (drop 1 indices)))))]
    [rooms edges dungeon-bounds]))

;; end of room generation stuff

(defn tilemap [width height initial-element]
  (doall (vec (repeat height (doall (vec (repeat width initial-element)))))))

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
                     (if (point-in-rectangle?
                          (let [[rx ry rw rh] room]
                            [(inc rx) (inc ry) (- rw 2) (- rh 2)])
                          [x1 y1])
                       \.
                       \#)
                     tile)) tilemap))
;; (defn paint-line [tilemap [x1 y1] [x2 y2]]
;;   )
(defn paint-horizontal-line [tilemap [x1 y1] x2 character]
  (let [delta-x (if (>= x2 x1) 1 -1)]
    (loop [tilemap tilemap
           x x1]
      (if (= x x2)
        tilemap
        (recur (assoc-in tilemap [y1 x] character)
               (+ x delta-x))))))
(defn paint-vertical-line [tilemap [x1 y1] y2 character]
  (let [delta-y (if (>= y2 y1) 1 -1)]
    (loop [tilemap tilemap
           y y1]
      (if (= y y2)
        tilemap
        (recur (assoc-in tilemap [y x1] character)
               (+ y delta-y))))))
(defn paint-corridor [tilemap [rectangle-a rectangle-b] direction]
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
                                    (and (tilemap-in-bounds? graph point)
                                         (or (and (= (tilemap-get tilemap point) \#)
                                                  (or (point-in-rectangle? rectangle-a point)
                                                      (point-in-rectangle? rectangle-b point)))
                                             (not (= (tilemap-get tilemap point) \#)))))
                                  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))]
    (reduce (fn [tilemap [x y]]
              (assoc-in tilemap [y x] \.))
            tilemap
            path-to-center)
    ;; (case direction
    ;;   :vertical (-> tilemap
    ;;                 (paint-vertical-line [x1 y1] y2 \.)
    ;;                 (paint-horizontal-line [x1 y2] x2 \.))
    ;;   :horizontal (-> tilemap
    ;;                   (paint-horizontal-line [x1 y1] x2 \.)
    ;;                   (paint-vertical-line [x2 y1] y2 \.)))
    ))
(defn paint-rooms-and-edges [[rooms edges [bx by bw bh]]]
  (as-> (tilemap bw bh \space) tilemap
    (reduce paint-room tilemap rooms)
    (reduce #(paint-corridor %1 [(get rooms (first %2))
                                 (get rooms (second %2))]
                             :vertical
                             ;; (rand-nth [:vertical :horizontal])
                             ) tilemap edges)))


(defonce test-room (tunneling-dungeon-gen 16 [0 0 80 48]))
(defonce painted-map (paint-rooms-and-edges test-room))

(defn make-game-state[]
  {:player {:position [1 1] :visual {:symbol \@ :foreground white :background black}}
   :entities [{:position [2 2] :visual {:symbol \@ :foreground green :background black}}
              {:position [4 4] :visual {:symbol \@ :foreground green :background black}}]
   :dungeon painted-map
   ;; [[\# \# \# \# \# \# \# \#]
   ;;  [\# \. \. \. \. \. \. \#]
   ;;  [\# \. \. \. \. \. \. \#]
   ;;  [\# \. \. \. \. \. \. \#]
   ;;  [\# \. \. \. \. \. \. \#]
   ;;  [\# \. \. \. \. \. \. \#]
   ;;  [\# \. \. \. \. \. \. \#]
   ;;  [\# \# \# \# \# \# \# \#]]
   })

;; These are grid aligned.
(defn draw-character! [canvas-context camera character point foreground-color background-color]
  (let [[x y] (game-coordinates->screen camera point)]
    (canvas/fill-rectangle! canvas-context [x y 8 16] background-color)
    (canvas/draw-text! canvas-context character [x y] "Dina" foreground-color 16)))

(defn draw-entity! [canvas-context camera entity light-sources]
  (let [{:keys [symbol foreground background]} (:visual entity)]
    (draw-character! canvas-context camera symbol (:position entity)
                     (color-lighting (:position entity) foreground light-sources) background)))

(defn draw-tilemap! [canvas-context camera tilemap light-sources]
  (let [[width height] (tilemap-dimensions tilemap)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [character (get-in tilemap [y x] \?)
              base-color (case character
                           \# white
                           \. red
                           black)
              color (if light-sources
                      (color-lighting [x y] base-color light-sources)
                      base-color)]
          (when-not (< (average color)
                       (if (= base-color white)
                         215
                         100))
            (draw-character! canvas-context camera character [x y]
                             color
                             black)))))))

(defn movement-direction [input]
  (cond (input/event-keydown input "ArrowUp") :up
        (input/event-keydown input "ArrowDown") :down
        (input/event-keydown input "ArrowLeft") :left
        (input/event-keydown input "ArrowRight") :right))

(defn move [position direction]
  (case direction
    :up (update position 1 dec)
    :down (update position 1 inc)
    :left (update position 0 dec)
    :right (update position 0 inc)
    position))

(defn player-handle [] {:type :player})
(defn entity-handle [id] {:type :entity :index id})
;; unordered, for iteration, for combat if initiative is ever a thing, sort by that.
(defn entity-handles [state]
  (into [(player-handle)] (vec (map-indexed #(entity-handle %1) (:entities state)))))

(defn lookup-entity [state handle]
  (case (:type handle)
    :player (:player state)
    :entity (get (:entities state) (:index handle))))

;; move to have handles to things to make things more convenient again.
(defn try-move [state position direction]
  (let [new-position (move position direction)
        stepped-tile? (= (get-in (:dungeon state) new-position nil) \#)        
        stepped-entity? (some (fn [entity-handle]
                                (when (= (:position (lookup-entity state entity-handle)) new-position)
                                  entity-handle))
                              (entity-handles state))]
    (when (and direction (or stepped-entity? stepped-tile?))
      {:tile stepped-tile?
       :entity stepped-entity?})))

(defn player-update [entity state input]
  (update entity :position
          (fn [position]
            (let [obstruction (try-move state position (movement-direction input))]
              (if obstruction
                position
                (move position (movement-direction input)))))))

(defn state-update [state input delta-time ticks]
  (as-> state state
    (update state :player #(player-update % state input))))

(defn light-sources [state time]
  [;; {:position [(+ (* (Math/sin (/ time 1000)) 2.5) 2.5) 4]
   ;;  :power 3.8
   ;;  :color white
   ;;  :ambient-strength 0.0085}
   {:position [30 20]
    :power (* (+ (Math/sin (/ time 1000)) 3) 10)
    :color white
    :ambient-strength 0.0085}])

(defn visibility-status [average-lighting]
  (condp <= average-lighting
    255 :definitely-visible
    200 :visible
    170 :barely-visible
    140 :blending-in-shadows
    100 :hiding-in-shadows
    90 :nearly-invisible
    :in-shadows))

(defn state-draw [canvas-context state input ticks]
  (canvas/clear-screen! canvas-context black)
  (let [camera [0 3]
        light-sources (light-sources state ticks)]
    ;; (println (:dungeon state))
    (draw-tilemap! canvas-context camera (:dungeon state) light-sources)
    ;; (draw-tilemap! canvas-context camera painted-map light-sources)
    (doseq [entity (into (:entities state) [(:player state)])]
      (draw-entity! canvas-context camera entity light-sources))
    ;; (doseq [[x y] (graph-search/breadth-first-search
    ;;                (:dungeon state)
    ;;                (:position (:player state))
    ;;                (:position (first (:entities state)))
    ;;                (fn [graph [x y]]
    ;;                  (filter #(tilemap-good-neighbor? graph %)
    ;;                          [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))]
    ;;   (canvas/fill-rectangle! canvas-context
    ;;                           (into (game-coordinates->screen camera [x y]) [8 16])
    ;;                           [0 255 0 128]))
    (let [[x y] (:position (:player state))
          light-average (average (color-lighting [x y] white light-sources))]
      (canvas/draw-text! canvas-context (str "Lighting Avg: " light-average) [0 0] "Dina" white 16)
      (canvas/draw-text! canvas-context (visibility-status light-average) [0 16] "Dina" white 16))
    ))

(defn game-loop [game-state time]
  (try 
    (do
      (.requestAnimationFrame js/window
                              (fn [current-time]
                                (game-loop
                                 (state-update game-state
                                               @input-state
                                               (max (/ (- current-time time) 1000) (/ 1 60))
                                               current-time)
                                 current-time)))
      (state-draw game-canvas-context game-state @input-state time)
      (swap! input-state input/new-frame))
    (catch :default exception
      (println exception)
      (.alert js/window exception))))

(defn setup-main-game-loop! []
  (let [input-event-handlers (input/make-default-handlers input-state)]
    (.addEventListener js/document "keydown" (:keydown input-event-handlers))
    (.addEventListener js/document "keyup" (:keyup input-event-handlers))
    (game-loop (make-game-state) 0)))

(defn init [] (setup-main-game-loop!))
