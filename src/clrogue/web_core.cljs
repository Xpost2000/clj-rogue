(ns clrogue.web-core
  (:require [clrogue.game :as game])
  (:require [clrogue.canvas :as canvas])
  (:require [clrogue.input :as input]))

(defonce input-state (atom (input/new)))
(defonce game-canvas-context (.getContext (.getElementById js/document "game-canvas") "2d"))

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
      (swap! input-state input/new-frame))
    (catch :default exception
      (println exception)
      (.alert js/window exception))))

(defn setup-main-game-loop! []
  (let [input-event-handlers (input/make-default-handlers input-state)]
    (.addEventListener js/document "keydown" (:keydown input-event-handlers))
    (.addEventListener js/document "keyup" (:keyup input-event-handlers))
    (game-loop (game/make-game-state) 0)))

(defn init [] (setup-main-game-loop!))
