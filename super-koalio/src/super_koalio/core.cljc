(ns super-koalio.core
  (:require [super-koalio.utils :as utils]
            [super-koalio.move :as move]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj  [super-koalio.tile :as tile :refer [read-tiled-map]]
               :cljs [super-koalio.tile :as tile :refer-macros [read-tiled-map]])))

(def koala-width 18)
(def koala-height 26)

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}
                       :x-velocity 0
                       :y-velocity 0
                       :player-x 20
                       :player-y 0
                       :player-width 1
                       :player-height (/ koala-height koala-width)
                       :can-jump? false
                       :started? false
                       :direction :right
                       :player-images {}
                       :player-walk-keys [:walk1 :walk2 :walk3]
                       :player-image-key :jump
                       :tiled-map nil
                       :tiled-map-entity nil
                       :camera (e/->camera)}))

(def tiled-xml (read-tiled-map "level1.tmx"))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load koalio image
  (utils/get-image "koalio.png"
    (fn [{:keys [data width height]}]
      (let [entity (c/compile game (e/->image-entity game data width height))
            images (vec (for [i (range 5)]
                          (t/crop entity
                             (* i koala-width)
                             0
                             koala-width
                             koala-height)))
            [stand jump walk1 walk2 walk3] images]
        ;; add it to the state
        (swap! *state update :player-images assoc
          :stand stand
          :jump jump
          :walk1 walk1
          :walk2 walk2
          :walk3 walk3))))
  ;; load the tiled map
  (tile/load-tiled-map game tiled-xml
    (fn [tiled-map entity]
      (swap! *state assoc :tiled-map tiled-map :tiled-map-entity entity))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [{:keys [pressed-keys
                player-x
                player-y
                player-width
                player-height
                direction
                player-images
                player-image-key
                tiled-map
                tiled-map-entity
                camera]
         :as state} @*state
        game-width (utils/get-width game)
        game-height (utils/get-height game)
        offset (/ game-width 2)
        tile-size (/ game-height (:map-height tiled-map))
        player-x (* player-x tile-size)
        player-y (* player-y tile-size)
        player-width (* player-width tile-size)
        player-height (* player-height tile-size)
        camera (t/translate camera (- player-x offset) 0)]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; render the tiled map
    (when tiled-map-entity
      (c/render game (-> tiled-map-entity
                         (t/project game-width game-height)
                         (t/camera camera)
                         (t/scale
                           (* (/ (:width tiled-map-entity)
                                 (:height tiled-map-entity))
                              game-height)
                           game-height))))
    ;; get the current player image to display
    (when-let [player (get player-images player-image-key)]
      ;; render the player
      (c/render game
        (-> player
            (t/project game-width game-height)
            (t/camera camera)
            (t/translate (cond-> player-x
                                 (= direction :left)
                                 (+ player-width))
              player-y)
            (t/scale (cond-> player-width
                             (= direction :left)
                             (* -1))
              player-height)))
      ;; change the state to move the player
      (swap! *state
        (fn [state]
          (->> state
               (move/move game)
               (move/prevent-move)
               (move/animate game))))))
  ;; return the game map
  game)

