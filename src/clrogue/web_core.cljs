(ns clrogue.web-core
  (:require [clrogue.game :as game])
  (:require [clrogue.canvas :as canvas])
  (:require [clrogue.input :as input]))

(defonce input-state (atom (input/new)))
(defonce game-canvas-context (.getContext (.getElementById js/document "game-canvas") "2d"))

;; to allow a forced redraw on resize
(def game-state-current-atom (atom nil))
(defn game-loop [game-state time]
  (try 
    (do
      (.requestAnimationFrame js/window
                              (fn [current-time]
                                (game-loop
                                 (game/state-update game-state
                                                    @input-state
                                                    (max (/ (- current-time time) 1000) (/ 1 60))
                                                    current-time)
                                 current-time)))
      (game/state-draw game-canvas-context game-state @input-state time)
      (swap! input-state input/new-frame)
      (reset! game-state-current-atom game-state))
    (catch :default exception
      (println exception)
      (.alert js/window exception))))

(def logical-resolution [800 600])
(defn on-resize-handler [event]
  (let [window-width (.-innerWidth js/window)
        window-height (.-innerHeight js/window)
        aspect-ratio (/ window-width window-height)
        logical-aspect-ratio (/ (first logical-resolution)
                                (second logical-resolution))]
    (println (str window-width " x " window-height))
    (when @game-state-current-atom
      (let [appropriate-scale
            (- (if (> aspect-ratio logical-aspect-ratio)
                 (/ window-height (second logical-resolution))
                 (/ window-width (first logical-resolution)))
               0.2)]
        (println appropriate-scale)
        (set! (.-width (.-canvas game-canvas-context)) (* (first logical-resolution) appropriate-scale))
        (set! (.-height (.-canvas game-canvas-context)) (* (second logical-resolution) appropriate-scale))
        (.scale game-canvas-context appropriate-scale appropriate-scale))
      (.requestAnimationFrame js/window
                              (fn [current-time]
                                (game/state-draw game-canvas-context
                                                 @game-state-current-atom
                                                 @input-state
                                                 current-time
                                                 true))))))

(defn setup-main-game-loop! [game-state]
  ;; (on-resize-handler nil)
  ;; (.setTimeout
  ;;  (fn []
  ;;    )
  ;;  300)
  (let [input-event-handlers (input/make-default-handlers input-state)]
    (.addEventListener js/window "resize" (fn [event] (on-resize-handler event)))
    (.addEventListener js/document "keydown" (:keydown input-event-handlers))
    (.addEventListener js/document "keyup" (:keyup input-event-handlers))
    (game-loop game-state 0)))

(defn init []
  (let [game-state (game/make-game-state)]
    (reset! game-state-current-atom game-state)
    (on-resize-handler nil)
    (js/setTimeout #(setup-main-game-loop! game-state) 100)))
