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

(defn draw-entity! [canvas-context camera entity]
  (let [{:keys [symbol foreground background]} (:visual entity)]
    (draw-character! canvas-context camera symbol (:position entity) foreground background)))

(defn draw-tilemap! [canvas-context camera tilemap]
  (let [width (count tilemap)
        height (count tilemap)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [character (get-in tilemap [y x] \?)]
          (draw-character! canvas-context camera character [x y]
                           (case character
                             \# white
                             \. red
                             black)
                           black))))))

(defn state-draw [canvas-context input state]
  (canvas/clear-screen! canvas-context black)
  (let [camera [0 0]]
    (draw-tilemap! canvas-context camera (:dungeon state))
    (doseq [entity (into (:entities state) [(:player state)])]
      (draw-entity! canvas-context camera entity))
    ))

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

;; move to have handles to things to make things more convenient again.
(defn try-move [state position direction]
  (let [new-position (move position direction)
        stepped-tile? (= (get-in (:dungeon state) new-position nil) \#)        
        stepped-player? (= (:position (:player state)) new-position)
        stepped-entity? (some (fn [[index entity]]
                                (when (= (:position entity) new-position)
                                  index))
                              (map-indexed vector (:entities state)))]
    (when (or stepped-entity? stepped-tile? stepped-player?)
      {:tile stepped-entity?
       :player stepped-player?
       :entity-index stepped-entity?})))

(defn entity-update [entity state]
  entity)

(defn player-update [entity state input]
  (update entity :position
          (fn [position]
            (let [obstruction (try-move state position (movement-direction input))]
              (if obstruction
                position
                (move position (movement-direction input)))))))

(defn state-update [state input delta-time]
  (as-> state state
    (update state :player #(player-update % state input))
    (update state :entities #(entity-update % state))))

(defn game-loop [game-state time]
  (.requestAnimationFrame js/window
                          (fn [current-time]
                            (game-loop
                             (state-update game-state
                                           @input-state
                                           (max (/ (- current-time time) 1000) (/ 1 60)))
                             current-time)))
  (state-draw game-canvas-context @input-state game-state)
  (swap! input-state input/new-frame))

(defn setup-main-game-loop! []
  (let [input-event-handlers (input/make-default-handlers input-state)]
    (.addEventListener js/document "keydown" (:keydown input-event-handlers))
    (.addEventListener js/document "keyup" (:keyup input-event-handlers))
    (game-loop (make-game-state) 0)))

(defn init [] (setup-main-game-loop!))
