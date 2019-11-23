(ns dungeon-crawler.core
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.move :as move]
            [dungeon-crawler.entities :as entities #?@(:cljs [:refer Entity])]
            [clojure.edn :as edn]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [clara.rules :as clara]
            [clarax.rules :as clarax]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]])
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj  [dungeon-crawler.tiles :as tiles :refer [read-tiled-map]]
               :cljs [dungeon-crawler.tiles :as tiles :refer-macros [read-tiled-map]]))
  #?(:clj (:import [dungeon_crawler.entities Entity])))

(def parsed-tiled-map (edn/read-string (read-tiled-map "level1.tmx")))
(def orig-camera (e/->camera true))
(def vertical-tiles 7)
(def max-attack-distance 1)
(def min-attack-interval 0.25)

(defrecord Game [total-time delta-time context])
(defrecord Window [width height])
(defrecord Camera [camera window player min-y max-y])
(defrecord Mouse [x y world-coords button])
(defrecord Keys [pressed])
(defrecord TiledMap [layers width height entities])
(defrecord Attack [source-id target-id])

(defn update-camera [window player]
  (let [{game-width :width game-height :height} window
        scaled-tile-size (/ game-height vertical-tiles)
        x (:x player)
        y (:y player)
        offset-x (/ game-width 2 scaled-tile-size)
        offset-y (/ game-height 2 scaled-tile-size)
        min-y (- y offset-y 1)
        max-y (+ y offset-y)]
    {:window window
     :camera (t/translate orig-camera (- x offset-x) (- y offset-y))
     :player player
     :min-y min-y
     :max-y max-y}))

(def *session (-> {:get-game
                   (fn []
                     (let [game Game]
                       game))
                   :get-window
                   (fn []
                     (let [window Window]
                       window))
                   :get-camera
                   (fn []
                     (let [camera Camera]
                       camera))
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
                         window Window
                         keys Keys
                         mouse Mouse
                         player Entity
                         :when (and (not= (:game player) game)
                                    (= (:char-type player) :player))]
                     (let [[xv yv :as v] (move/get-player-velocity window (:pressed keys) mouse player)]
                       (when (or (not= 0 xv) (not= 0 yv))
                         (clarax/merge! player (-> (move/move v player game)
                                                   (assoc :game game :direction nil))))))
                   :update-camera
                   (let [window Window
                         :when (or (pos? (:width window))
                                   (pos? (:height window)))
                         player Entity
                         :when (= (:char-type player) :player)
                         camera Camera
                         :when (or (not= (:window camera) window)
                                   (not= (:player camera) player))]
                     (clarax/merge! camera (update-camera window player)))
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
                              (clarax/merge! entity)))
                   :player-attack-with-key
                   (let [game Game
                         player Entity
                         :when (and (= (:char-type player) :player)
                                    (-> (:total-time game)
                                        (- (:last-attack player))
                                        (>= min-attack-interval)))
                         keys Keys
                         :when (contains? (:pressed keys) :space)
                         target [Entity]
                         :when (not= (:char-type target) :player)]
                     (clarax/merge! player {:last-attack (:total-time game)})
                     (some->> target
                              (mapv #(vector (move/calc-distance % player) %))
                              (sort-by first)
                              (filter #(<= (first %) max-attack-distance))
                              first
                              second
                              :id
                              (->Attack (:id player))
                              clara/insert-unconditional!))
                   :player-attack-with-mouse
                   (let [game Game
                         player Entity
                         :when (and (= (:char-type player) :player)
                                    (-> (:total-time game)
                                        (- (:last-attack player))
                                        (>= min-attack-interval)))
                         mouse Mouse
                         :when (= (:button mouse) :right)
                         target [Entity]
                         :when (not= (:char-type target) :player)]
                     (clarax/merge! player {:last-attack (:total-time game)})
                     (some->> target
                              (mapv #(vector (move/calc-distance % (:world-coords mouse)) %))
                              (sort-by first)
                              (filter #(<= (first %) max-attack-distance))
                              first
                              second
                              :id
                              (->Attack (:id player))
                              clara/insert-unconditional!))
                   :update-mouse-world-coords
                   (let [window Window
                         mouse Mouse
                         :when (= nil (:world-coords mouse))
                         player Entity
                         :when (= (:char-type player) :player)]
                     (let [{:keys [x y]} mouse
                           {:keys [width height]} window
                           ;; convert mouse coords to (-1 to 1) coords
                           matrix (->> (m/projection-matrix width height)
                                       (m/multiply-matrices 3 (m/translation-matrix x y)))
                           wx (nth matrix 6)
                           wy (* -1 (nth matrix 7))
                           ;; convert to tile coords
                           y-multiplier (/ vertical-tiles 2)
                           x-multiplier (* y-multiplier (/ width height))
                           wx (* wx x-multiplier)
                           wy (* wy y-multiplier)
                           ;; make mouse relative to player position
                           wx (+ wx (:x player))
                           wy (+ wy (:y player))]
                       (clarax/merge! mouse {:world-coords {:x wx :y wy}})))
                   :attack
                   (let [attack Attack
                         source Entity
                         :when (= (:id source) (:source-id attack))
                         target Entity
                         :when (= (:id target) (:target-id attack))]
                     (when (<= (move/calc-distance source target)
                               max-attack-distance)
                       (println (:char-type source) "attacked" (:char-type target)))
                     (clara/retract! attack))}
                  ->session
                  (clara/insert
                    (->Mouse 0 0 nil nil)
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
            (clarax/merge session $ {:x x :y y :world-coords nil})
            (clara/fire-rules $)))))

(defn update-window-size! [width height]
  (swap! *session
    (fn [session]
      (as-> session $
            (clara/query $ :get-window)
            (clarax/merge session $ {:width width :height height})
            (clara/fire-rules $)))))

(defn load-entities [game]
  (doseq [{:keys [path instances] :as spawn-data} entities/spawn-data]
    (utils/get-image path
      (fn [image]
        (swap! *session
          (fn [session]
            (->> instances
                 (reduce
                   (fn [session instance]
                     (clara/insert session (entities/->entity game spawn-data image instance)))
                   session)
                 clara/fire-rules)))))))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; insert game record
  (swap! *session
    (fn [session]
      (-> session
          (clara/insert
            (map->Game game)
            (->Window (utils/get-width game) (utils/get-height game))
            (->Camera orig-camera nil nil 0 0))
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
        {game-width :width game-height :height :as window} (clara/query session :get-window)
        {:keys [camera min-y max-y]} (clara/query session :get-camera)]
    (when (and window (pos? game-width) (pos? game-height))
      (let [scaled-tile-size (/ game-height vertical-tiles)]
        ;; render the background
        (c/render game (update screen-entity :viewport
                               assoc :width game-width :height game-height))
        ;; get the current player image to display
        (when-let [{:keys [x y width height current-image]} player]
          (let [entities (->> (:entities tiled-map)
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
                  entities)))))
    ;; insert/update the game record
    (if-let [game' (clara/query session :get-game)]
      (swap! *session
        (fn [session]
          (-> session
              (clarax/merge game' game)
              clara/fire-rules)))
      (init game)))
  ;; return the game map
  game)

