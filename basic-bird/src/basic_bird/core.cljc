(ns basic-bird.core
  (:require [basic-bird.utils :as utils]
            [basic-bird.move :as move]
            [basic-bird.session :as session]
            [basic-bird.entities :as entities]
            [odoyle.rules :as o]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(defn update-pressed-keys! [f k]
  (swap! session/*session
    (fn [session]
      (let [pressed-keys (-> session
                             (o/query-all ::session/keys)
                             first
                             :pressed
                             (f k))]
        (o/insert session ::session/keys ::session/pressed pressed-keys)))))

(defn update-window-size! [width height]
  (swap! session/*session o/insert ::session/window {::session/width width ::session/height height}))

(defn update-mouse-button! [button]
  (swap! session/*session o/insert ::session/mouse ::session/button button))

(defn update-mouse-coords! [x y]
  (swap! session/*session o/insert ::session/mouse {::session/x x ::session/y y}))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; initialize session
  (reset! session/*session
    (-> session/initial-session
        (o/insert ::session/keys ::session/pressed #{})
        (o/insert ::session/player (entities/->player))
        (o/insert ::session/window
                  {::session/width (utils/get-width game)
                   ::session/height (utils/get-height game)})
        (o/insert ::session/mouse
                  {::session/x 0
                   ::session/y 0
                   ::session/button nil})
        o/fire-rules))
  ;; load images and put them in the session
  (doseq [[k path] {:walk1 "player_walk1.png"
                    :walk2 "player_walk2.png"
                    :walk3 "player_walk3.png"}]
    (utils/get-image path
      (fn [{:keys [data width height]}]
        (let [;; create an image entity (a map with info necessary to display it)
              entity (e/->image-entity game data width height)
              ;; compile the shaders so it is ready to render
              entity (c/compile game entity)
              ;; assoc the width and height to we can reference it later
              entity (assoc entity :width width :height height)]
          ;; add it to the session
          (swap! session/*session
                 (fn [session]
                   (o/fire-rules
                     (o/insert session
                       ::session/player
                       ::entities/images
                       (-> (o/query-all session ::session/player)
                           first
                           :images
                           (assoc k entity)))))))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn tick [game]
  (when @session/*reload?
    (reset! session/*reload? false)
    (init game))
  (let [;; update the time and fire the rules
        session (swap! session/*session
                  (fn [session]
                    (-> session
                        (o/insert ::session/time
                                  {::session/total (:total-time game)
                                   ::session/delta (:delta-time game)})
                        o/fire-rules)))
        ;; query the data we need to render
        {game-width :width game-height :height :as window} (first (o/query-all session ::session/window))
        ;; the mouse is not currently being used
        ;mouse (first (o/query-all session ::session/mouse))
        player (first (o/query-all session ::session/player))]
    (when (and (pos? game-width) (pos? game-height))
      ;; render the blue background
      (c/render game (update screen-entity :viewport
                             assoc :width game-width :height game-height))
      ;; render the player
      (when-let [player-image (:current-image player)]
        (c/render game
          (-> player-image
              (t/project game-width game-height)
              (t/translate (cond-> (:x player)
                                   (= (:direction player) :left)
                                   (+ (:width player)))
                           (:y player))
              (t/scale (cond-> (:width player)
                               (= (:direction player) :left)
                               (* -1))
                       (:height player)))))))
  ;; return the game map
  game)

