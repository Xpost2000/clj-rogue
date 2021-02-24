(ns clrogue.game
  (:require [clrogue.canvas :as canvas])
  (:require [clrogue.input :as input])
  (:require [clrogue.graph-search :as graph-search])
  (:require [clrogue.dungeon-generation :as dungeon-generation])
  (:require [clrogue.tilemap :as tilemap]))
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
  (if sources
    (reduce vector-add
            (mapv #(contribution base-color % position euclidean-distance) sources))
    base-color))

(defn game-coordinates->screen [[camera-x camera-y] [x y]]
  [(* (+ x camera-x) 8) (* (+ y camera-y) 16)])
(defn screen-coordinates->game [[camera-x camera-y] [x y]] :not-done)

(defn make-entity [position visual]
  {:position position
   :visual visual
   :speed 1
   :turn-time 2
   :wait-time 1})


(defn make-game-state[]
  {:player (make-entity [1 1] {:symbol \@ :foreground white :background black})
   :entities [(make-entity [2 2] {:symbol \@ :foreground green :background black})
              (make-entity [4 4] {:symbol \@ :foreground green :background black})]
   :turn-tracker []
   :dungeon (dungeon-generation/paint-rooms-and-edges (dungeon-generation/tunneling 4 [0 0 50 30]))})

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
  (let [[width height] (tilemap/dimensions tilemap)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [character (tilemap/at tilemap [x y])
              base-color (case character
                           \# white
                           \. red
                           black)
              color (color-lighting [x y] base-color light-sources)]
          (when-not (< (average color)
                       (if (= base-color white)
                         215
                         100))
            (draw-character! canvas-context camera character [x y] color black)))))))

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

(defn entities [state]
  (map #(lookup-entity state %) (entity-handles state)))

;; move to have handles to things to make things more convenient again.
(defn try-move [state position direction]
  (let [new-position (move position direction)
        stepped-tile? (= (tilemap/at (:dungeon state) new-position) \# \#)        
        stepped-entity? (some (fn [entity-handle]
                                (when (= (:position (lookup-entity state entity-handle)) new-position)
                                  entity-handle))
                              (entity-handles state))]
    (when (and direction (or stepped-entity? stepped-tile?))
      {:tile stepped-tile?
       :entity stepped-entity?})))

(defn move-entity [entity state direction]
  (update entity :position
          (fn [position]
            (let [obstruction (try-move state position direction)]
              (if obstruction position (move position direction))))))

(defn player-update [entity state input]
  (move-entity entity state (movement-direction input)))

(defn update-entity [state handle f]
  (case (:type handle)
    :player (update state :player f)
    :entity (update-in state [:entities (:index handle)] f)
    state))

(defn update-entities [state f]
  (reduce #(update-entity %1 %2 f) state (entity-handles state)))

(defn turns [entity] (:speed entity))
(defn cooldown [entity] 1)

(defn has-turn? [entity] (> (:turn-time entity) 0))

(defn entity-maybe-new-round [entity]
  (letfn [(start-turn? [] (and (= (:turn-time entity) 0)
                               (= (:wait-time entity) 0)))]
    (if (start-turn?)
      (-> entity
          (assoc :turn-time (turns entity))
          (assoc :wait-time (cooldown entity)))
      entity)))

(defn entity-wait-until-turn [entity]
  (if (and (> (:wait-time entity) 0)
           (= (:turn-time entity) 0))
    (update entity :wait-time dec)
    entity))

(defn entity-end-round [entity]
  (if (has-turn? entity)
    (update entity :turn-time dec)
    entity))

(defn new-round [state]
  (as-> (update-entities state entity-maybe-new-round) state
    (update state :turn-tracker
            (fn [tracker]
              (reduce
               (fn [turn-tracker-content entity-handle]
                 (let [entity (lookup-entity state entity-handle)]
                   (if (has-turn? entity) 
                     (conj turn-tracker-content entity-handle)
                     turn-tracker-content)))
               tracker
               (entity-handles state))))))

(defn turn-tracker-run [state]
  state)

(defn end-round [state]
  (as-> state state
    (update-entity state (first (:turn-tracker state)) entity-end-round)
    (update state :turn-tracker
            (fn [tracker]
              (filterv
               #(has-turn? (lookup-entity state %))
               tracker)))))

(defn state-update [state input delta-time ticks]
  (as-> state state
    (if (empty? (:turn-tracker state))
      (new-round state)
      state)
    ;; (turn-tracker-run state)

    (if (input/event-keydown input "w")
      (end-round state)
      state)

    (if (empty? (:turn-tracker state))
      (update-entities state entity-wait-until-turn)
      state)
    ;; (end-round state)
    
    ;; (update state :player #(player-update % state input))
    ))

(defn light-sources [state time]
  [;; {:position [(+ (* (Math/sin (/ time 1000)) 2.5) 2.5) 4]
   ;;  :power 3.8
   ;;  :color white
   ;;  :ambient-strength 0.0085}
   {:position [30 20]
    :power (* (+ (Math/sin (/ time 1000)) 3) 999)
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
    (draw-tilemap! canvas-context camera (:dungeon state) nil)
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
    (doseq [[row entity-handle] (map-indexed vector (:turn-tracker state))]
      (let [real-entity (lookup-entity state entity-handle)]
        (canvas/draw-text! canvas-context
                           (str entity-handle " ttime: " (:turn-time real-entity) " wtime: " (:wait-time real-entity))
                           [0 (* row 16)] "Dina" white 16)))
    (doseq [[row entity-handle] (map-indexed vector (entity-handles state))]
      (let [real-entity (lookup-entity state entity-handle)]
        (canvas/draw-text! canvas-context
                           (str entity-handle " ttime: " (:turn-time real-entity) " wtime: " (:wait-time real-entity))
                           [400 (* row 16)] "Dina" white 16)))
    ;; (let [[x y] (:position (:player state))
    ;;       light-average (average (color-lighting [x y] white light-sources))]
    ;;   (canvas/draw-text! canvas-context (str "Lighting Avg: " light-average) [0 0] "Dina" white 16)
    ;;   (canvas/draw-text! canvas-context (visibility-status light-average) [0 16] "Dina" white 16))
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
