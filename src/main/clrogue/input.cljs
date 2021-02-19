(ns clrogue.input)
;; TODO start using event.key not event.code

(defn new [] {:events []})
(defn new-frame [input] (assoc input :events []))
(defn key-event [type keycode] {:type type :key keycode})

(defn event-keydown [input keycode]
  (some #(= % (key-event :keydown keycode)) (:events input)))
(defn event-keyup [input keycode]
  (some #(= % (key-event :keyup keycode)) (:events input)))

(defn shift-down? [input] (event-keydown input "Shift"))
(defn make-default-keydown-handler [input-state]
  (fn [event]
    (.preventDefault event)
    (swap! input-state
           (fn [last]
             (let [events (:events last)]
               {:events (conj events (key-event :keydown (.-key event)))})))))
(defn make-default-keyup-handler [input-state]
  (fn [event]
    (.preventDefault event)
    (swap! input-state
           (fn [last]
             (let [events (:events last)]
               {:events (conj events (key-event :keyup (.-code event)))})))))

(defn keys-down [input-state]
  (map #(:key %) (filter #(= :keydown (:type %)) (:events input-state))))

(defn first-key-down [input-state]
  (let [keys (keys-down input-state)]
    (when keys (first keys))))

(defn any-event-of [input-state type]
  (> (count (filter #(= type (:type %)) (:events input-state))) 0))

(defn make-default-handlers [input-state]
  {:keydown (make-default-keydown-handler input-state)
   :keyup (make-default-keyup-handler input-state)})
