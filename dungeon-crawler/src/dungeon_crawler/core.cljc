(ns dungeon-crawler.core
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.move :as move]
            [clojure.edn :as edn]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj  [dungeon-crawler.tile :as tile :refer [read-tiled-map]]
               :cljs [dungeon-crawler.tile :as tile :refer-macros [read-tiled-map]])))
  
(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :mouse-button nil
                       :pressed-keys #{}
                       :characters {}
                       :tiled-map nil
                       :tiled-map-entity nil
                       :camera (e/->camera true)}))

(def tiled-map (edn/read-string (read-tiled-map "level1.tmx")))

(def vertical-tiles 15)

(defn create-grid [image tile-size mask-size]
  (let [offset (-> tile-size (- mask-size) (/ 2))]
    (vec (for [y (range 0 (:height image) tile-size)]
           (vec (for [x (range 0 (:width image) tile-size)]
                  (t/crop image (+ x offset) (+ y offset) mask-size mask-size)))))))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load the tiled map
  (tile/load-tiled-map game tiled-map
    (fn [tiled-map entity]
      (swap! *state assoc :tiled-map tiled-map :tiled-map-entity entity)
      ;; load images and put them in the state atom
      (doseq [[k path] {:player "characters/male_light.png"}]
        (utils/get-image path
          (fn [{:keys [data width height]}]
            (let [entity (e/->image-entity game data width height)
                  entity (c/compile game entity)
                  tile-size 256
                  mask-size 128
                  grid (create-grid entity tile-size mask-size)
                  moves (zipmap move/directions
                          (map #(vec (take 4 %)) grid))
                  attacks (zipmap move/directions
                            (map #(nth % 4) grid))
                  specials (zipmap move/directions
                             (map #(nth % 5) grid))
                  hits (zipmap move/directions
                         (map #(nth % 6) grid))
                  deads (zipmap move/directions
                          (map #(nth % 7) grid))
                  character {:moves moves
                             :attacks attacks
                             :specials specials
                             :hits hits
                             :deads deads
                             :direction :s
                             :current-image (get-in moves [:s 0])
                             :width (/ mask-size tile-size)
                             :height (/ mask-size tile-size)
                             :x 5
                             :y 30
                             :x-velocity 0
                             :y-velocity 0}]
              ;; add it to the state
              (swap! *state update :characters assoc k character))))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [{:keys [pressed-keys
                characters
                tiled-map
                tiled-map-entity
                camera]
         :as state} @*state
        game-width (utils/get-width game)
        game-height (utils/get-height game)
        offset-x (/ game-width 2)
        offset-y (/ game-height 2)
        tile-size (/ game-height vertical-tiles)]
    ;; render the background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; get the current player image to display
    (when-let [player (:player characters)]
      (let [player-x (* (:x player) tile-size)
            player-y (* (:y player) tile-size)
            player-width (* (:width player) tile-size)
            player-height (* (:height player) tile-size)
            camera (t/translate camera (- player-x offset-x) (- player-y offset-y))]
        ;; render the tiled map
        (when tiled-map-entity
          (c/render game (-> tiled-map-entity
                             (t/project game-width game-height)
                             (t/camera camera)
                             (t/scale
                               (* tile-size (:map-width tiled-map))
                               (* tile-size (:map-height tiled-map))))))
        ;; render the player
        (when-let [image (:current-image player)]
          (c/render game
            (-> image
                (t/project game-width game-height)
                (t/camera camera)
                (t/translate player-x player-y)
                (t/scale player-width player-height))))
        ;; change the state to move the player
        (swap! *state update-in [:characters :player]
          (fn [player]
            (->> player
                 (move/move game state)
                 (move/prevent-move tiled-map)
                 (move/animate game)))))))
  ;; return the game map
  game)

