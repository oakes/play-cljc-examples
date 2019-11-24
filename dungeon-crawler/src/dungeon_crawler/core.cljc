(ns dungeon-crawler.core
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.entities :as entities]
            [dungeon-crawler.session :as session]
            [clojure.edn :as edn]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [clara.rules :as clara]
            [clarax.rules :as clarax]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj  [dungeon-crawler.tiles :as tiles :refer [read-tiled-map]]
               :cljs [dungeon-crawler.tiles :as tiles :refer-macros [read-tiled-map]])))

;; this is a perf optimization.
;; while we could call clara/query with these keywords directly,
;; saving them to global vars will avoid the hash lookup inside the game loop.
(let [query-fns (clarax/query-fns @session/*session)]
  (def get-game (:get-game query-fns))
  (def should-restart? (:should-restart? query-fns))
  (def get-player (:get-player query-fns))
  (def get-enemies (:get-enemies query-fns))
  (def get-tiled-map (:get-tiled-map query-fns))
  (def get-window (:get-window query-fns))
  (def get-camera (:get-camera query-fns))
  (def get-keys (:get-keys query-fns))
  (def get-mouse (:get-mouse query-fns))
  (def get-enemy-under-cursor (:get-enemy-under-cursor query-fns)))

(defn update-pressed-keys! [f k]
  (swap! session/*session
    (fn [session]
      (as-> session $
            (get-keys $)
            (clarax/merge session $ (update $ :pressed f k))
            (clara/fire-rules $)))))

(defn update-mouse-button! [button]
  (swap! session/*session
    (fn [session]
      (as-> session $
            (get-mouse $)
            (clarax/merge session $ {:button button})
            (clara/fire-rules $)))))

(defn update-mouse-coords! [x y]
  (-> (swap! session/*session
        (fn [session]
          (as-> session $
                (get-mouse $)
                (clarax/merge session $ {:x x :y y :world-coords nil})
                (clara/fire-rules $))))
      get-enemy-under-cursor))

(defn update-window-size! [width height]
  (swap! session/*session
    (fn [session]
      (as-> session $
            (get-window $)
            (clarax/merge session $ {:width width :height height})
            (clara/fire-rules $)))))

(def parsed-tiled-map (edn/read-string (read-tiled-map "level1.tmx")))

(defn load-entities [game]
  (doseq [{:keys [path instances] :as spawn-data} entities/spawn-data]
    (utils/get-image path
      (fn [image]
        (swap! session/*session
          (fn [session]
            (->> instances
                 (reduce
                   (fn [session instance]
                     (clara/insert session (session/map->Entity (entities/->entity game spawn-data image instance))))
                   session)
                 clara/fire-rules)))))))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; insert game record
  (swap! session/*session
    (fn [session]
      (-> session
          (clara/insert
            (session/map->Game game)
            (session/->Window (utils/get-width game) (utils/get-height game))
            (session/->Camera session/orig-camera nil nil 0 0))
          clara/fire-rules)))
  ;; load the tiled map
  (tiles/load-tiled-map game parsed-tiled-map
    (fn [tiled-map]
      (swap! session/*session
        (fn [session]
          (-> session
              (clara/insert (session/map->TiledMap tiled-map))
              clara/fire-rules)))
      (load-entities game))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 150 255) (/ 150 255) (/ 150 255) 1] :depth 1}})

(defn tick [game]
  (let [session @session/*session
        session (if (should-restart? session)
                  (session/restart!)
                  session)
        player (get-player session)
        enemies (get-enemies session)
        tiled-map (get-tiled-map session)
        {game-width :width game-height :height :as window} (get-window session)
        {:keys [camera min-y max-y]} (get-camera session)]
    (when (and window (pos? game-width) (pos? game-height))
      (let [scaled-tile-size (/ game-height session/vertical-tiles)]
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
    (if-let [game' (get-game session)]
      (swap! session/*session
        (fn [session]
          (-> session
              (clarax/merge game' game)
              clara/fire-rules)))
      (init game)))
  ;; return the game map
  game)

