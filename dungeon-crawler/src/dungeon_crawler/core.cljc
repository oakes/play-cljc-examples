(ns dungeon-crawler.core
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.entities :as entities]
            [dungeon-crawler.session :as session]
            [dungeon-crawler.move :as move]
            [clojure.edn :as edn]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [odoyle.rules :as o]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj  [dungeon-crawler.tiles :as tiles :refer [read-tiled-map]]
               :cljs [dungeon-crawler.tiles :as tiles :refer-macros [read-tiled-map]])))

(defn update-pressed-keys! [f k]
  (swap! session/*session
    (fn [session]
      (let [pressed-keys (-> session
                             (o/query-all ::session/keys)
                             first
                             :pressed
                             (f k))]
        (o/insert session ::session/keys ::session/pressed pressed-keys)))))

(defn update-mouse-button! [button]
  (swap! session/*session o/insert ::session/mouse ::session/button button))

(defn update-mouse-coords! [x y]
  (-> (swap! session/*session o/insert ::session/mouse {::session/x x ::session/y y})
      session/get-enemy-under-cursor))

(defn update-window-size! [width height]
  (swap! session/*session o/insert ::session/window {::session/width width ::session/height height}))

(def parsed-tiled-map (edn/read-string (read-tiled-map "level1.tmx")))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; initialize session
  (reset! session/*session
    (-> session/initial-session
        (o/insert ::session/keys
                  {::session/pressed #{}})
        (o/insert ::session/mouse
                  {::session/x 0
                   ::session/y 0
                   ::session/world-x 0
                   ::session/world-y 0
                   ::session/button :none})
        (o/insert ::session/window
                  {::session/width (utils/get-width game)
                   ::session/height (utils/get-height game)})
        (o/insert ::session/camera
                  {::session/camera session/orig-camera
                   ::session/min-y 0
                   ::session/max-y 0})
        o/fire-rules))
  ;; load entities
  (doseq [{:keys [path instances] :as spawn-data} entities/spawn-data]
    (utils/get-image path
      (fn [image]
        (swap! session/*session
          (fn [session]
            (->> instances
                 (reduce
                   (fn [session instance]
                     (let [e (entities/->entity game spawn-data image instance)
                           id (swap! entities/*latest-id inc)]
                       (-> session
                           (o/insert id e)
                           (o/insert id ::session/distance-from-cursor (inc move/max-cursor-distance))
                           (o/insert id ::session/distance-from-player (inc move/max-aggro-distance))
                           o/fire-rules)))
                   session)))))))
  ;; load tiled map
  (tiles/load-tiled-map game parsed-tiled-map
    (fn [tiled-map]
      (swap! session/*session
             (fn [session]
               (-> session
                   (o/insert ::tiles/tiled-map tiled-map)
                   o/fire-rules)))))
  ;; return game map
  game)

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 150 255) (/ 150 255) (/ 150 255) 1] :depth 1}})

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
        ;; run queries
        entities (o/query-all session ::session/entity)
        tiled-map (first (o/query-all session ::session/tiled-map))
        {game-width :width game-height :height :as window} (first (o/query-all session ::session/window))
        {:keys [camera min-y max-y]} (first (o/query-all session ::session/camera))]
    (when (and (pos? game-width) (pos? game-height))
      (let [scaled-tile-size (/ game-height session/vertical-tiles)]
        ;; render the background
        (c/render game (update screen-entity :viewport
                               assoc :width game-width :height game-height))
        ;; render the entities
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
                                                  (t/invert camera))))))
                            (concat (for [{:keys [x y width height current-image]} entities
                                          :when (< min-y y max-y)]
                                      [y (-> current-image
                                             (t/project game-width game-height)
                                             (t/scale scaled-tile-size scaled-tile-size)
                                             (t/invert camera)
                                             (t/translate x y)
                                             (t/scale width height))]))
                            (sort-by first <)
                            vec)]
          (run! (fn [[y-pos entity]]
                  (c/render game entity))
                entities)))))
  ;; return the game map
  game)

