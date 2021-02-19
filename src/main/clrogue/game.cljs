(ns clrogue.game
  (:require [clrogue.canvas :as canvas])
  (:require [clrogue.input :as input]))

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

(defn vector-add [a b] (map + a b))
(defn vector-subtract [a b] (map - a b))
(defn vector-scale [a b] (map #(* % b) a))
(defn vector-component-multiply [a b] (map * a b))

(defn contribution [source to distance-function]
  (vector-scale white
                (* 4.0 (attenuation 6.5 (distance-function to source)))))

(def ambient-lighting-power 0.1)
(defn color-lighting [position base-color sources]
  (reduce vector-add
          (vector-scale base-color ambient-lighting-power)
          (mapv (fn [source]
                  (contribution source position euclidean-distance))
                sources)))

(defn make-game-state[]
  {:player {:position [1 1] :visual {:symbol \@ :foreground white :background black}}
   :entities [{:position [2 2] :visual {:symbol \@ :foreground green :background black}}
              {:position [4 4] :visual {:symbol \@ :foreground green :background black}}]
   :dungeon [[\# \# \# \# \# \# \# \#]
             [\# \. \. \. \. \. \. \#]
             [\# \. \. \. \. \. \. \#]
             [\# \. \. \. \. \. \. \#]
             [\# \. \. \. \. \. \. \#]
             [\# \. \. \. \. \. \. \#]
             [\# \. \. \. \. \. \. \#]
             [\# \# \# \# \# \# \# \#]]})

;; These are grid aligned.
(defn draw-character! [canvas-context [camera-x camera-y] character [x y] foreground-color background-color]
  (let [[x y] [(+ (* x 8) camera-x) (+ (* y 16) camera-y)]]
    (canvas/fill-rectangle! canvas-context [x y 8 16] background-color)
    (canvas/draw-text! canvas-context character [x y] "Dina" foreground-color 16)))

(defn draw-entity! [canvas-context camera entity light-sources]
  (let [{:keys [symbol foreground background]} (:visual entity)]
    (draw-character! canvas-context camera symbol (:position entity)
                     (color-lighting (:position entity) foreground light-sources) background)))

(defn draw-tilemap! [canvas-context camera tilemap light-sources]
  (let [width (count tilemap)
        height (count tilemap)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [character (get-in tilemap [y x] \?)]
          (draw-character! canvas-context camera character [x y]
                           (color-lighting [x y]
                                           (case character
                                             \# white
                                             \. red
                                             black)
                                           light-sources)
                           black))))))

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

(defn light-sources [time]
  [[(+ (* (Math/sin (/ time 1000)) 2.5) 2.5) 4]
   ;; [6 2]
   ])

(defn state-draw [canvas-context state input ticks]
(canvas/clear-screen! canvas-context black)
(let [camera [0 0]]
  (draw-tilemap! canvas-context camera (:dungeon state) (light-sources ticks))
  (doseq [entity (into (:entities state) [(:player state)])]
    (draw-entity! canvas-context camera entity (light-sources ticks)))))

(defn game-loop [game-state time]
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

(defn setup-main-game-loop! []
(let [input-event-handlers (input/make-default-handlers input-state)]
  (.addEventListener js/document "keydown" (:keydown input-event-handlers))
  (.addEventListener js/document "keyup" (:keyup input-event-handlers))
  (game-loop (make-game-state) 0)))

(defn init [] (setup-main-game-loop!))
