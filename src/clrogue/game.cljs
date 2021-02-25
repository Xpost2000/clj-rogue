(ns clrogue.game
  (:require [clrogue.canvas :as canvas])
  (:require [clrogue.input :as input])
  (:require [clrogue.graph-search :as graph-search])
  (:require [clrogue.dungeon-generation :as dungeon-generation])
  (:require [clrogue.tilemap :as tilemap]))
                                        ;
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

(defn health-percent [entity]
  (/ (:health entity) (:max-health entity)))
(defn alive? [entity]
  (> (:health entity) 0))
(defn make-entity
  ([position visual speed]
   {:name "guy"
    :max-health 20
    :health 20
    :position position
    :visual visual
    :speed speed
    :turn-time speed
    :wait-time 1})
  ([position visual] (make-entity position visual 1)))
(defn make-player [position]
  (assoc (make-entity position {:symbol \@ :foreground white :background black} 1)
         :player? true
         :name "hero"
         :health 15))

(defn make-game-state[]
  {:player (make-player [1 1])
   :entities [(make-entity [4 7] {:symbol \@ :foreground green :background black} 2)
              (make-entity [4 9] {:symbol \@ :foreground green :background black})
              (make-entity [4 10] {:symbol \@ :foreground green :background black})]
   :turn-tracker []
   :dungeon (dungeon-generation/paint-rooms-and-edges (dungeon-generation/tunneling 8 [0 0 50 30]))})

;; These are grid aligned.
(defn draw-character! [canvas-context camera character point foreground-color background-color]
  (let [[x y] (game-coordinates->screen camera point)]
    (canvas/fill-rectangle! canvas-context [x y 8 16] background-color)
    (canvas/draw-text! canvas-context character [x y] "Dina" foreground-color 16)))

(defn draw-entity! [canvas-context camera entity light-sources]
  (let [{:keys [symbol foreground background]} (:visual entity)]
    (draw-character! canvas-context camera symbol (:position entity)
                     (color-lighting (:position entity)
                                     (if (alive? entity) foreground [40 40 40 255])
                                     light-sources) background)))

(defn draw-tilemap! [canvas-context camera tilemap light-sources]
  (let [[width height] (tilemap/dimensions tilemap)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [character (tilemap/at tilemap [x y])
              base-color (case character
                           \# white
                           \. (vector-scale white 0.7)
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

(defn try-move [state position direction]
  (let [new-position (move position direction)
        stepped-tile? (= (tilemap/at (:dungeon state) new-position) \# \#)        
        stepped-entity? (some (fn [entity-handle]
                                (let [entity (lookup-entity state entity-handle)]
                                  (when (and (= (:position entity) new-position) (alive? entity))
                                    entity-handle)))
                              (entity-handles state))]
    (when (and direction (or stepped-entity? stepped-tile?))
      {:tile stepped-tile?
       :entity stepped-entity?})))

(defn damage-entity [entity state dmg]
  (update entity :health - dmg))
(defn move-entity [entity state direction]
  (update entity :position
          (fn [position]
            (let [obstruction (try-move state position direction)]
              (if obstruction position (move position direction))))))

(defn update-entity [state handle f]
  (case (:type handle)
    :player (update state :player f)
    :entity (update-in state [:entities (:index handle)] f)
    state))

(defn update-entities [state f]
  (reduce #(update-entity %1 %2 f) state (entity-handles state)))

(defn message-color [message]
  (case (:type message)
    :informative white
    :combat red
    [0 255 255 255]))
(defn message [state message type]
  (update state :message-log #(conj % {:type type :text message :time 3.5})))
(def informative-message #(message %1 %2 :informative))

(defn update-messages [state delta-time]
  (update state :message-log
          (fn [message-log]
            (filterv #(> (:time %) 0.0)
                     (map #(update % :time - delta-time)
                          message-log)))))

(defn turns [entity] (:speed entity))
(defn cooldown [entity] 1)

(defn has-turn? [entity] (> (:turn-time entity) 0))
(defn multiple-turns? [entity] (> (:turn-time entity) 1))

(defn entity-maybe-new-round [entity]
  (letfn [(start-turn? [] (and (= (:turn-time entity) 0)
                               (= (:wait-time entity) 0)))]
    (if (start-turn?)
      (-> entity
          (assoc :turn-time (turns entity))
          (assoc :wait-time (cooldown entity)))
      entity)))

(defn entity-wait-for-turn [entity]
  (if (and (> (:wait-time entity) 0)
           (= (:turn-time entity) 0))
    (update entity :wait-time dec)
    entity))

(defn entity-end-round [entity]
  (if (has-turn? entity)
    (update entity :turn-time dec)
    entity))

(def wait-action identity)
(defn move-action [actor move-direction]
  (fn [state]
    (update-entity state actor #(move-entity % state move-direction))))
;; player versions of actions have additional message logging.
;; I could also just send the messages to the entities themselves I guess...
;; Or make this a multimethod? idk
(defn player-move-action [actor move-direction]
  (fn [state]
    (let [entity (lookup-entity state actor)]
      (if-let [obstacle (try-move state (:position entity) move-direction)]
       	(informative-message state
                             (cond (:tile obstacle) "You bumped into a wall."
                                   (:entity obstacle) "You bumped into a thing."))
        (update-entity state actor #(move-entity % state move-direction))))))
(defn player-melee-combat-action [actor other-actor]
  (fn [state]
    (let [actor-entity (lookup-entity state actor)
          other-actor-entity (lookup-entity state other-actor)]
      (let [random-damage (+ (rand-int 4) 2)]
        (as-> state state
          (informative-message state (str (:name actor-entity) " does " random-damage " dmg to " (:name other-actor-entity)))
          (update-entity state other-actor #(damage-entity % state random-damage)))))))

;; I know I have pathfinding but it's expensive to run so this is going to be an idiot bump
;; also only four direction movement
(defn chase-range? [entity target-position]
  (<= (euclidean-distance (:position entity) target-position) 5))
(defn melee-attack-range? [entity target-position]
  (<= (euclidean-distance (:position entity) target-position) 1.414213562))
(defn direction-to-move-to [state start-position end-position]
  (let [direction-vector (vector-subtract end-position start-position)
        direction (cond (> (first direction-vector) 0) :right
                        (< (first direction-vector) 0) :left
                        (> (second direction-vector) 0) :down
                        (< (second direction-vector) 0) :up)]
    direction))
(defn entity-turn-action [actor state input]
  (let [self-entity (lookup-entity state actor)
        movement-direction (if (chase-range? self-entity (:position (:player state)))
                             (direction-to-move-to state (:position self-entity) (:position (:player state)))
                             (rand-nth [:up :down :left :right]))
        random-direction (rand-nth [:up :down :left :right])]
    (if-let [obstacle (try-move state (:position self-entity) movement-direction)]
      (cond (and (:entity obstacle)
                 (= (:type (:entity obstacle)) :player))
            (let [obstacle-entity (lookup-entity state (:entity obstacle))]
              (if (and (melee-attack-range? self-entity
                                            (:position (:player state)))
                       (alive? (:player state)))
                (player-melee-combat-action actor (:entity obstacle))
                (move-action actor random-direction)))
            :else
            (move-action actor random-direction))
      (move-action actor movement-direction))))
(defn player-turn-action [actor state input]
  (let [move-direction (movement-direction input)
        self-entity (lookup-entity state actor)]
    (cond move-direction
          (if-let [obstacle (try-move state (:position self-entity) move-direction)]
            (if-let [other-entity (:entity obstacle)]
              (player-melee-combat-action actor other-entity)
              (player-move-action actor move-direction))
            (player-move-action actor move-direction))
          (input/event-keydown input ".") wait-action)))

(defn turn-action [actor state input]
  (case (:type actor)
    :player (player-turn-action actor state input)
    :entity (entity-turn-action actor state input)))

(defn clean-turn-tracker [state]
  (update state :turn-tracker
          (fn [tracker]
            (filterv
             #(and (has-turn? (lookup-entity state %))
                   (alive? (lookup-entity state %)))
             tracker))))

(defn run-round [state action actor]
  (-> state
      action
      (update-entity actor entity-end-round)))

(defn new-round [state]
  (as-> (update-entities state entity-maybe-new-round) state
    (update state :turn-tracker
            (fn [tracker]
              (reduce
               (fn [turn-tracker-content entity-handle]
                 (let [entity (lookup-entity state entity-handle)]
                   (if (and (has-turn? entity)
                            (alive? entity)) 
                     (conj turn-tracker-content entity-handle)
                     turn-tracker-content)))
               tracker
               (entity-handles state))))))

(defn end-round [state input]
  (reduce
   (fn [new-state current-actor]
     (if-let [action (turn-action current-actor new-state input)]
       (let [new-state (run-round new-state action current-actor)
             entity (lookup-entity new-state current-actor)]
         (cond (and (multiple-turns? entity)
                    (not (:player? entity))) ; avoids burning all of the player's turns instantly.
               (recur new-state current-actor)
               :else new-state))
       (reduced state)))
   state
   (:turn-tracker state)))

(defn player-alive? [state] (alive? (:player state)))
(defn state-update [state input delta-time ticks]
  (if (player-alive? state)
    (as-> state state
      (if (empty? (:turn-tracker state))
        (new-round state)
        (end-round state input))
      (clean-turn-tracker state)
      (update-messages state delta-time)
      (if (empty? (:turn-tracker state))
        (update-entities state entity-wait-for-turn)
        state))
    (as-> state state
      (update-messages state delta-time))))

(defn light-sources [state time]
  [;; {:position [(+ (* (Math/sin (/ time 1000)) 2.5) 2.5) 4]
   ;;  :power 3.8
   ;;  :color white
   ;;  :ambient-strength 0.0085}
   {:position [10 10]
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

(defn state-ui-draw [canvas-context state input ticks]
  (if (player-alive? state)
    (do
      (doseq [[row message] (map-indexed vector (:message-log state))]
        (canvas/draw-text! canvas-context
                           (:text message)
                           [0 (* row 16)]
                           "Dina"
                           (assoc (message-color message)
                                  3 (* 255 (/ (:time message) 1.0)))
                           16))
      (let [light-sources (light-sources state ticks)
            player-entity (:player state)]
        (canvas/draw-text! canvas-context
                           (str "HP:" (:health player-entity))
                           [380 0]
                           "Dina"
                           (let [health-percent (health-percent player-entity)]
                             (cond
                               (<= health-percent 0.25) red
                               (<= health-percent 0.5) [255 255 0 255]
                               (<= health-percent 0.75) [0 100 255 255]
                               :else green))
                           16)
        (canvas/draw-text! canvas-context
                           (case (visibility-status (average (color-lighting (:position player-entity)
                                                                             white
                                                                             light-sources)))
	                         :definitely-visible "VISIBLE!"                     
                             :visible "VISIBLE"
                             :barely-visible "BARELY VISIBLE"
                             :blending-in-shadows "HIDDEN?"
                             :hiding-in-shadows "HIDDEN"
                             :nearly-invisible "INVISIBLE"
                             :in-shadows "SHADOW!"
                             "SAFE?")
                           [380 16]
                           "Dina"
                           white
                           16)))
    (do
      (canvas/fill-rectangle! canvas-context [0
                                              0
                                              (canvas/width canvas-context)
                                              (canvas/height canvas-context)]
                              (assoc red 3 128))
      (canvas/draw-text! canvas-context
                         "DEATH"
                         [300 200]
                         "Dina"
                         white
                         64))))

(defn state-draw [canvas-context state input ticks]
  (canvas/clear-screen! canvas-context black)
  (let [camera [0 3]
        light-sources (light-sources state ticks)]
    (draw-tilemap! canvas-context camera (:dungeon state) light-sources)
    (doseq [entity (into (:entities state) [(:player state)])]
      (draw-entity! canvas-context camera entity light-sources))
    (state-ui-draw canvas-context state input ticks)))
