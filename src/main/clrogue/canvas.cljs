(ns clrogue.canvas)

(defn width [canvas-context] (.-width (.-canvas canvas-context)))
(defn height [canvas-context] (.-height (.-canvas canvas-context)))

(defn clear-screen! [canvas-context [r g b a]]
  (set! (.-globalAlpha canvas-context) (str a))
  (set! (.-fillStyle canvas-context) (str "rgba(" r "," g "," b "," a")"))
  (.beginPath canvas-context)
  (.rect canvas-context 0 0 (width canvas-context) (height canvas-context))
  (.fill canvas-context))

(defn fill-rectangle! [canvas-context [x y w h] [r g b a]]
  (set! (.-globalAlpha canvas-context) (str a))
  (set! (.-fillStyle canvas-context) (str "rgba(" r "," g "," b "," a")"))
  (.beginPath canvas-context)
  (.rect canvas-context x y w h)
  (.fill canvas-context))

(defn draw-rectangle!
  ([canvas-context [x y w h] [r g b a] thickness]
   (set! (.-lineWidth canvas-context) thickness)
   (set! (.-globalAlpha canvas-context) (str a))
   (set! (.-strokeStyle canvas-context) (str "rgba(" r "," g "," b "," a")"))
   (.beginPath canvas-context)
   (.rect canvas-context x y w h)
   (.stroke canvas-context))
  ([canvas-context rectangle color] (draw-rectangle! canvas-context rectangle color 1)))

(defn draw-line!
  ([canvas-context [x y x1 y1] [r g b a] thickness]
   (set! (.-lineWidth canvas-context) thickness)
   (set! (.-globalAlpha canvas-context) (str a))
   (set! (.-strokeStyle canvas-context) (str "rgba(" r "," g "," b "," a")"))
   (.beginPath canvas-context)
   (.moveTo canvas-context x y)
   (.lineTo canvas-context x1 y1)
   (.stroke canvas-context))
  ([canvas-context line color] (draw-line! canvas-context line color 1)))

(defn draw-text! [canvas-context text [x y aligned?] font [r g b a] size]
  (set! (.-font canvas-context) (str (str size) "px " font))
  (set! (.-globalAlpha canvas-context) (str a))
  (set! (.-fillStyle canvas-context) (str "rgb(" r "," g "," b ")"))
  (.fillText canvas-context text x (+ y size)))
