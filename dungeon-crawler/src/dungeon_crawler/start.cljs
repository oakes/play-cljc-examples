(ns dungeon-crawler.start
  (:require [dungeon-crawler.core :as c]
            [play-cljc.gl.core :as pc]
            [goog.events :as events]))

(defn resize [{:keys [context] :as game}]
  (let [display-width context.canvas.clientWidth
        display-height context.canvas.clientHeight]
    (when (or (not= context.canvas.width display-width)
              (not= context.canvas.height display-height))
      (set! context.canvas.width display-width)
      (set! context.canvas.height display-height))))

(defn game-loop [game]
  (resize game)
  (let [game (c/tick game)]
    (js/requestAnimationFrame
      (fn [ts]
        (let [ts (* ts 0.001)]
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
      (swap! c/*state
        (fn [state]
          (let [bounds (.getBoundingClientRect canvas)
                x (- (.-clientX event) (.-left bounds))
                y (- (.-clientY event) (.-top bounds))]
            (assoc state :mouse-x x :mouse-y y))))))
  (events/listen js/window "mousedown"
    (fn [event]
      (swap! c/*state
        (fn [state]
          (assoc state :mouse-button (mousecode->keyword (.-button event)))))))
  (events/listen js/window "mouseup"
    (fn [event]
      (swap! c/*state
        (fn [state]
          (assoc state :mouse-button nil))))))

(defn keycode->keyword [keycode]
  (condp = keycode
    37 :left
    39 :right
    38 :up
    40 :down
    nil))

(defn listen-for-keys []
  (events/listen js/window "keydown"
    (fn [event]
      (when-let [k (keycode->keyword (.-keyCode event))]
        (swap! c/*state update :pressed-keys conj k))))
  (events/listen js/window "keyup"
    (fn [event]
      (when-let [k (keycode->keyword (.-keyCode event))]
        (swap! c/*state update :pressed-keys disj k)))))

;; start the game

(defonce context
  (let [canvas (js/document.querySelector "canvas")
        context (.getContext canvas "webgl2")
        initial-game (assoc (pc/->game context)
                            :delta-time 0
                            :total-time 0)]
    (set! (.-oncontextmenu canvas) (fn [e] (.preventDefault e)))
    (listen-for-mouse canvas)
    (listen-for-keys)
    (c/init initial-game)
    (game-loop initial-game)
    context))

