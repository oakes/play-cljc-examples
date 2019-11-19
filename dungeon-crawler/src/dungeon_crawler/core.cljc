(ns dungeon-crawler.core
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.move :as move]
            [clojure.edn :as edn]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [clara.rules :as clara]
            [clarax.rules :as clarax]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]])
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj  [dungeon-crawler.tiles :as tiles :refer [read-tiled-map]]
               :cljs [dungeon-crawler.tiles :as tiles :refer-macros [read-tiled-map]])))

(defrecord Game [total-time delta-time context])
(defrecord Mouse [x y button])
(defrecord Keys [pressed])
(defrecord Entity [name
                   moves
                   attacks
                   specials
                   hits
                   deads
                   direction
                   current-image
                   width
                   height
                   x
                   y
                   x-change
                   y-change
                   x-velocity
                   y-velocity
                   game])
(defrecord TiledMap [layers width height entities])

(def *session (-> {:get-game
                   (fn []
                     (let [game Game]
                       game))
                   :get-mouse
                   (fn []
                     (let [mouse Mouse]
                       mouse))
                   :get-keys
                   (fn []
                     (let [keys Keys]
                       keys))
                   :get-entity
                   (fn [?name]
                     (let [entity Entity
                           :when (= (:name entity) ?name)]
                       entity))
                   :get-tiled-map
                   (fn []
                     (let [tiled-map TiledMap]
                       tiled-map))
                   :move-player
                   (let [game Game
                         keys Keys
                         mouse Mouse
                         entity Entity
                         :when (and (not= (:game entity) game)
                                    (= (:name entity) :player))]
                     (clarax/merge! entity (-> entity
                                               (move/move game (:pressed keys) mouse)
                                               (move/animate game)
                                               (assoc :game game))))
                   :dont-overlap-tile
                   (let [tiled-map TiledMap
                         entity Entity
                         :when (or (not (== 0 (:x-change entity)))
                                   (not (== 0 (:y-change entity))))]
                     (let [{:keys [x y
                                   width height
                                   x-change y-change]} entity
                           old-x (- x x-change)
                           old-y (- y y-change)
                           touching-x? (tiles/touching-tile? tiled-map "walls" x old-y width height)
                           touching-y? (tiles/touching-tile? tiled-map "walls" old-x y width height)]
                       (when (or touching-x? touching-y?)
                         (clarax/merge! entity (cond-> {:x-change 0 :y-change 0}
                                                       touching-x?
                                                       (assoc :x-velocity 0 :x old-x)
                                                       touching-y?
                                                       (assoc :y-velocity 0 :y old-y))))))}
                  ->session
                  (clara/insert
                    (->Game 0 0 0)
                    (->Mouse 0 0 nil)
                    (->Keys #{}))
                  clara/fire-rules
                  atom))

(defn update-pressed-keys! [f k]
  (swap! *session
    (fn [session]
      (as-> session $
            (clara/query $ :get-keys)
            (clarax/merge session $ (update $ :pressed f k))
            (clara/fire-rules $)))))

(defn update-mouse-button! [button]
  (swap! *session
    (fn [session]
      (as-> session $
            (clara/query $ :get-mouse)
            (clarax/merge session $ {:button button})
            (clara/fire-rules $)))))

(defn update-mouse-coords! [x y]
  (swap! *session
    (fn [session]
      (as-> session $
            (clara/query $ :get-mouse)
            (clarax/merge session $ {:x x :y y})
            (clara/fire-rules $)))))

(def tiled-map (edn/read-string (read-tiled-map "level1.tmx")))
(def camera (e/->camera true))
(def vertical-tiles 7)

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
  (tiles/load-tiled-map game tiled-map
    (fn [tiled-map]
      (swap! *session
        (fn [session]
          (-> session
              (clara/insert (map->TiledMap tiled-map))
              clara/fire-rules)))
      ;; load images and put them in the session
      (doseq [[char-name path] {:player "characters/male_light.png"}]
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
                  [x y] (tiles/isometric->screen 5 5)
                  character {:name char-name
                             :moves moves
                             :attacks attacks
                             :specials specials
                             :hits hits
                             :deads deads
                             :direction :s
                             :current-image (get-in moves [:s 0])
                             :width (/ mask-size tile-size)
                             :height (/ mask-size tile-size)
                             :x x
                             :y y
                             :x-change 0
                             :y-change 0
                             :x-velocity 0
                             :y-velocity 0}]
              ;; add it to the session
              (swap! *session
                (fn [session]
                  (-> session
                      (clara/insert (map->Entity character))
                      clara/fire-rules))))))))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 150 255) (/ 150 255) (/ 150 255) 1] :depth 1}})

(defn tick [game]
  (let [session @*session
        player (clara/query session :get-entity :?name :player)
        tiled-map (clara/query session :get-tiled-map)
        game-width (utils/get-width game)
        game-height (utils/get-height game)]
    (when (and (pos? game-width) (pos? game-height))
      (let [scaled-tile-size (/ game-height vertical-tiles)
            offset-x (/ game-width 2 scaled-tile-size)
            offset-y (/ game-height 2 scaled-tile-size)]
        ;; render the background
        (c/render game (update screen-entity :viewport
                               assoc :width game-width :height game-height))
        ;; get the current player image to display
        (when-let [{:keys [x y width height current-image]} player]
          (let [camera (t/translate camera (- x offset-x) (- y offset-y))
                min-y (- y offset-y 1)
                max-y (+ y offset-y)
                entities (->> (:entities tiled-map)
                              (remove (fn [[y-pos]]
                                        (or (< y-pos min-y)
                                            (> y-pos max-y))))
                              (mapv (fn [y-pos-and-entity]
                                      (update y-pos-and-entity 1
                                              (fn [entity]
                                                (-> entity
                                                    (t/project game-width game-height)
                                                    (t/scale scaled-tile-size scaled-tile-size)
                                                    (t/camera camera))))))
                              (cons [y (-> current-image
                                           (t/project game-width game-height)
                                           (t/scale scaled-tile-size scaled-tile-size)
                                           (t/camera camera)
                                           (t/translate x y)
                                           (t/scale width height))])
                              (sort-by first <)
                              vec)]
            (run! (fn [[y-pos entity]]
                    (c/render game entity))
                  entities)
            ;; change the state to move the player
            (swap! *session
              (fn [session]
                (-> session
                    (clarax/merge (clara/query session :get-game) game)
                    clara/fire-rules))))))))
  ;; return the game map
  game)

