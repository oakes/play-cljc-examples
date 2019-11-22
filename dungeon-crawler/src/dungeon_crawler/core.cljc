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
(defrecord Entity [char-type
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
                   :get-player
                   (fn []
                     (let [entity Entity
                           :when (= (:char-type entity) :player)]
                       entity))
                   :get-enemies
                   (fn []
                     (let [entity [Entity]
                           :when (not= (:char-type entity) :player)]
                       entity))
                   :get-tiled-map
                   (fn []
                     (let [tiled-map TiledMap]
                       tiled-map))
                   :move-enemy
                   (let [game Game
                         player Entity
                         :when (= (:char-type player) :player)
                         entity Entity
                         :when (and (not= (:game entity) game)
                                    (not= (:char-type entity) :player))]
                     (clarax/merge! entity (-> (move/get-enemy-velocity entity player)
                                               (move/move entity game)
                                               (assoc :game game :direction nil))))
                   :move-player
                   (let [game Game
                         keys Keys
                         mouse Mouse
                         entity Entity
                         :when (and (not= (:game entity) game)
                                    (= (:char-type entity) :player))]
                     (clarax/merge! entity (-> (move/get-player-velocity game (:pressed keys) mouse entity)
                                               (move/move entity game)
                                               (assoc :game game :direction nil))))
                   :animate
                   (let [game Game
                         entity Entity
                         :when (= (:direction entity) nil)]
                     (some->> (move/animate entity game)
                              (clarax/merge! entity)))
                   :dont-overlap-tile
                   (let [tiled-map TiledMap
                         entity Entity
                         :when (or (not= 0 (:x-change entity))
                                   (not= 0 (:y-change entity)))]
                     (some->> (move/dont-overlap-tile entity tiled-map)
                              (clarax/merge! entity)))}
                  ->session
                  (clara/insert
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

(def parsed-tiled-map (edn/read-string (read-tiled-map "level1.tmx")))
(def camera (e/->camera true))
(def vertical-tiles 7)
(def tile-size 256)

(defn create-grid [image tile-size mask-size]
  (let [offset (-> tile-size (- mask-size) (/ 2))]
    (vec (for [y (range 0 (:height image) tile-size)]
           (vec (for [x (range 0 (:width image) tile-size)]
                  (t/crop image (+ x offset) (+ y offset) mask-size mask-size)))))))

(defn ->entity [entity char-type mask-size x y]
  (let [grid (create-grid entity tile-size mask-size)
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
        [x y] (tiles/isometric->screen x y)]
    (map->Entity
      {:char-type char-type
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
       :y-velocity 0})))

(def player-spawn-point {:x 2.5 :y 2.5})
(def spawn-points (for [row (range tiles/rows)
                        col (range tiles/cols)
                        :let [point {:x (-> row (* 10) (+ 2.5))
                                     :y (-> col (* 10) (+ 2.5))}]
                        :when (not= point player-spawn-point)]
                    point))

;; the entities are cached here so we don't constantly
;; make new ones when reloading this namespace
(defonce *entity-cache (atom {}))

(defn load-entities [game]
  (doseq [{:keys [path instances char-type mask-size]}
          [{:char-type :player
            :path "characters/male_light.png"
            :mask-size 128
            :instances [player-spawn-point]}
           {:char-type :ogre
            :path "characters/ogre.png"
            :mask-size 256
            :instances (->> spawn-points shuffle (take 5))}
           {:char-type :elemental
            :path "characters/elemental.png"
            :mask-size 256
            :instances (->> spawn-points shuffle (take 5))}]]
    (utils/get-image path
      (fn [{:keys [data width height]}]
        (let [entity (or (char-type @*entity-cache)
                         (->> (e/->image-entity game data width height)
                              (c/compile game)
                              (swap! *entity-cache assoc char-type)
                              char-type))]
          (doseq [{:keys [x y]} instances]
            (swap! *session
              (fn [session]
                (-> session
                    (clara/insert (->entity entity char-type mask-size x y))
                    clara/fire-rules)))))))))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; insert game record
  (swap! *session
    (fn [session]
      (-> session
          (clara/insert (map->Game game))
          clara/fire-rules)))
  ;; load the tiled map
  (tiles/load-tiled-map game parsed-tiled-map
    (fn [tiled-map]
      (swap! *session
        (fn [session]
          (-> session
              (clara/insert (map->TiledMap tiled-map))
              clara/fire-rules)))
      (load-entities game))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 150 255) (/ 150 255) (/ 150 255) 1] :depth 1}})

(defn tick [game]
  (let [session @*session
        player (clara/query session :get-player)
        enemies (clara/query session :get-enemies)
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
                              (concat (for [{:keys [x y width height current-image]} enemies
                                            :when (< min-y y max-y)]
                                        [y (-> current-image
                                               (t/project game-width game-height)
                                               (t/scale scaled-tile-size scaled-tile-size)
                                               (t/camera camera)
                                               (t/translate x y)
                                               (t/scale width height))]))
                              (sort-by first <)
                              vec)]
            (run! (fn [[y-pos entity]]
                    (c/render game entity))
                  entities))))
      ;; insert/update the game record
      (if-let [game* (clara/query session :get-game)]
        (swap! *session
          (fn [session]
            (-> session
                (clarax/merge game* game)
                clara/fire-rules)))
        (init game))))
  ;; return the game map
  game)

