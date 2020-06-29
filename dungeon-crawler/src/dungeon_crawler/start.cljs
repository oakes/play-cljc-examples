(ns dungeon-crawler.start
  (:require [dungeon-crawler.core :as c]
            [play-cljc.gl.core :as pc]
            [goog.events :as events]))

(defn msec->sec [n]
  (* 0.001 n))

(defn game-loop [game]
  (let [game (c/tick game)]
    (js/requestAnimationFrame
      (fn [ts]
        (let [ts (msec->sec ts)]
          (game-loop (assoc game
                            :delta-time (- ts (:total-time game))
                            :total-time ts)))))))

(defn mousecode->keyword [mousecode]
  (condp = mousecode
    0 :left
    2 :right
    nil))

(defn listen-for-mouse [canvas]
  (events/listen js/window "mousemove"
    (fn [event]
      (let [bounds (.getBoundingClientRect canvas)
            x (- (.-clientX event) (.-left bounds))
            y (- (.-clientY event) (.-top bounds))]
        (let [show-hand? (c/update-mouse-coords! x y)]
          (-> js/document .-body .-style .-cursor
              (set! (if show-hand? "pointer" "default")))))))
  (events/listen js/window "mousedown"
    (fn [event]
      (c/update-mouse-button! (mousecode->keyword (.-button event)))))
  (events/listen js/window "mouseup"
    (fn [event]
      (c/update-mouse-button! nil))))

(defn keycode->keyword [keycode]
  (condp = keycode
    32 :space
    37 :left
    39 :right
    38 :up
    40 :down
    nil))

(defn listen-for-keys []
  (events/listen js/window "keydown"
    (fn [event]
      (when-let [k (keycode->keyword (.-keyCode event))]
        (c/update-pressed-keys! conj k))))
  (events/listen js/window "keyup"
    (fn [event]
      (when-let [k (keycode->keyword (.-keyCode event))]
        (c/update-pressed-keys! disj k)))))

(defn resize [context]
  (let [display-width context.canvas.clientWidth
        display-height context.canvas.clientHeight]
    (set! context.canvas.width display-width)
    (set! context.canvas.height display-height)
    (c/update-window-size! display-width display-height)))

(defn listen-for-resize [context]
  (events/listen js/window "resize"
    (fn [event]
      (resize context))))

;; start the game

(defonce context
  (let [canvas (js/document.querySelector "canvas")
        context (.getContext canvas "webgl2")
        initial-game (assoc (pc/->game context)
                            :delta-time 0
                            :total-time (msec->sec (js/performance.now)))]
    (c/init initial-game)
    (set! (.-oncontextmenu canvas) (fn [e] (.preventDefault e)))
    (listen-for-mouse canvas)
    (listen-for-keys)
    (resize context)
    (listen-for-resize context)
    (game-loop initial-game)
    context))

