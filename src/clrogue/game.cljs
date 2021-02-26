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

(declare informative-message)
(declare update-entity)
(declare damage-entity)
(declare heal-entity)

(def game-entities
  {:human-base
   {:name "human"
    :max-health 20
    :health 20
   	:visual {:symbol \@
             :foreground white
             :background black}
    :speed 1}
   :player
   {:inherits :human-base :player? true}
   :zombie
   {:inherits :human-base
    :name "zombie guy"
    :visual {:foreground green}}
   :fast-zombie
   {:inherits :zombie
    :name "fast-zombie"
    :visual {:foreground red}
    :speed 10}})
(def game-items
  {:healing-potion
   {:name "potion of health"
    :on-use (fn [user-handle state]
              (update-entity state user-handle #(heal-entity % state 100)))}
   :fake-healing-potion
   {:inherits :healing-potion
    :on-use (fn [user-handle state]
              (informative-message state "phony potion"))}
   :death-potion
   {:inherits :healing-potion
    :on-use (fn [user-handle state]
              (update-entity state user-handle #(damage-entity % state 100)))}})

(declare query-from)
(declare query-for)
(defn parents-of [entity table]
  (loop [entity entity
         parents []]
    (if-let [parent-entity (query-for table (:inherits entity))]
      (recur parent-entity
             (conj parents (:inherits entity)))
      parents)))

(defn query-from
  ([table thing field]
   (if-let [query-value (get thing field)]
     (if (or (associative? query-value)
             (vector? query-value)
             (seq? query-value))
       (let [parent-fields (mapv #(query-for table % field)
                                 (parents-of thing table))]
         (apply merge (into parent-fields [query-value])))
       query-value)
     (when-let [parent (:inherits thing)]
       (query-for table parent field)))))

(defn query-for
  ([table id field] (query-from table (query-for table id) field))
  ([table id] (get table id)))

(defn localize-properties [table id properties]
  (let [base (query-for table id)]
    (reduce
     (fn [accumulator property]
       (if-let [property-lookup (query-for table id property)]
         (assoc accumulator property property-lookup)
         accumulator))
     {} properties)))

(defn item-usable? [item]
  (query-for game-items item :on-use))

(defn make-entity [position type]
  (into {:inherits type}
        (-> (localize-properties game-entities type [:speed :max-health :health])
            (assoc :position position
                   :turn-time (query-for game-entities type :speed)
                   :wait-time 1))))

(defn make-player [position]
  (-> (make-entity position :player) 
      (assoc :name "hero"
             :inventory [:fake-healing-potion
                         :death-potion
                         :healing-potion])))

(defn make-game-state[]
  {:player (make-player [1 1])
   :entities [(make-entity [4 7] :zombie)]
   :screen :gameplay
   :currently-selected-inventory-item 0

   :turn-tracker []

   :previous-game-time 0

   :game-time 0
   :dungeon (dungeon-generation/paint-rooms-and-edges (dungeon-generation/tunneling 3 [0 0 20 20]))})

;; These are grid aligned.
(defn draw-character! [canvas-context camera character point foreground-color background-color]
  (let [[x y] (game-coordinates->screen camera point)]
    (canvas/fill-rectangle! canvas-context [x y 8 16] background-color)
    (canvas/draw-text! canvas-context character [x y] "Dina" foreground-color 16)))

(defn draw-entity! [canvas-context camera entity light-sources]
  (let [{:keys [symbol foreground background]} (query-from game-entities entity :visual)]
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

(defmulti lookup-entity (fn [state handle] (:type handle)))
(defmethod lookup-entity :player [state handle]
  (:player state))
(defmethod lookup-entity :entity [state handle]
  (get-in state [:entities (:index handle)]))

(defmulti update-entity (fn [state handle f] (:type handle)))
(defmethod update-entity :player [state handle f]
  (update state :player f))
(defmethod update-entity :entity [state handle f]
  (update-in state [:entities (:index handle)] f))

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

(defn heal-entity [entity state amount]
  (update entity :health + amount))
(defn damage-entity [entity state dmg]
  (update entity :health - dmg))
(defn move-entity [entity state direction]
  (update entity :position
          (fn [position]
            (let [obstruction (try-move state position direction)]
              (if obstruction position (move position direction))))))

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
      (let [random-damage (+ (rand-int 4) 2)
            hit-roll (rand-int 20)]
        ;; (println (query-from game-entities other-actor-entity :health))
        ;; (println other-actor-entity)
        (if (> hit-roll 12)
          (as-> state state
            (informative-message state (str (query-from game-entities actor-entity :name) " does " random-damage " dmg to " (query-from game-entities other-actor-entity :name)))
            (update-entity state other-actor #(damage-entity % state random-damage)))
          (as-> state state
            (informative-message state (str (query-from game-entities actor-entity :name) " missed an attack against " (query-from game-entities other-actor-entity :name)))))))))

(defn use-item-action [actor item]
  (fn [state]
    (let [entity (lookup-entity state actor)
          on-use-fn (query-for game-items item :on-use)]
      (on-use-fn actor state))))

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
    (cond
      (= (:screen state) :inventory)
      (if (input/event-keydown input "Enter")
        (do
          (let [item-to-use (get-in self-entity [:inventory
                                                 (:currently-selected-inventory-item state)])]
            (if (item-usable? item-to-use)
              (use-item-action actor item-to-use)))))
      move-direction
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
               (entity-handles state))))
    (update state :game-time inc)))

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

(defn player-has-turn? [state] (= (:type (first (:turn-tracker state))) :player))
(defn player-alive? [state] (alive? (:player state)))
(defn handle-player-ui-interaction [state input]
  (cond (input/event-keydown input "i") (assoc state :screen :inventory) 
        (input/event-keydown input "s") (assoc state :screen :gameplay)
        (input/event-keydown input "ArrowDown") (update state :currently-selected-inventory-item inc)
        (input/event-keydown input "ArrowUp") (update state :currently-selected-inventory-item dec)
        :else state))

(defn clean-dead-entities [state]
  (update state :entities #(filterv alive? %)))

(defn state-update [state input delta-time ticks]
  (if (player-alive? state)
    (as-> (assoc state :previous-game-time (:game-time state)) state
      (if (empty? (:turn-tracker state))
        (new-round state)
        (end-round state input))
      (if (player-has-turn? state)
        (handle-player-ui-interaction state input)
        state)
      (clean-turn-tracker state)
      (clean-dead-entities state)
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
    :power (* (+ (Math/sin (/ (:game-time state) 10)) 3) 99)
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

(defn state-draw
  ([canvas-context state input ticks] (state-draw canvas-context state input ticks false))
  ([canvas-context state input ticks forced]
   (if (player-alive? state)
     (case (:screen state)
       :gameplay
       (when (or forced (not (= (:game-time state) (:previous-game-time state)))) 
         (canvas/fill-rectangle! canvas-context [0 0 800 600] black)
         (let [camera [0 3]
               light-sources (light-sources state ticks)]
           (draw-tilemap! canvas-context camera (:dungeon state) light-sources)
           (doseq [entity (into (:entities state) [(:player state)])]
             (draw-entity! canvas-context camera entity light-sources)))
         (state-ui-draw canvas-context state input ticks))
       :inventory (do
                    (canvas/fill-rectangle! canvas-context [0 0 800 600] black)
                    (let [player-entity (:player state)]
                      (doseq [[row item] (map-indexed vector (:inventory player-entity))]
                        (let [item-lookup (query-for game-items item :name)]
                          (canvas/draw-text! canvas-context
                                             (if (= (:currently-selected-inventory-item state) row)
                                               (str "=> " item-lookup)
                                               item-lookup)
                                             [0 (+ 48 (* 16 row))] "Dina" white 16))))))
     (do
       (canvas/fill-rectangle! canvas-context [0 0 800 600] black)
       (canvas/draw-text! canvas-context
                          "DEATH"
                          [300 200]
                          "Dina"
                          white
                          64)
       (canvas/draw-text! canvas-context
                          "Refresh for another run!"
                          [200 264]
                          "Dina"
                          white
                          32)))))
