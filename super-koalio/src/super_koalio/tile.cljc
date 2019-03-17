(ns super-koalio.tile
  (:require [super-koalio.utils :as utils]
            [play-cljc.transforms :as t]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            #?(:clj [clojure.java.io :as io])))

#?(:clj (defmacro read-tiled-map [fname]
          (slurp (io/resource (str "public/" fname)))))

(defn load-tiled-map [game parsed *state]
  (let [tileset (first (filter #(= :tileset (:tag %)) (:content parsed)))
        image (first (filter #(= :image (:tag %)) (:content tileset)))
        {{:keys [tilewidth tileheight]} :attrs} tileset]
    (utils/get-image (-> image :attrs :source)
      (fn [{:keys [data width height]}]
        (let [entity (c/compile game (e/->image-entity game data width height))]
          (swap! *state assoc :tiled-map-images
            (vec
              (for [col (range (/ width tilewidth))]
                (vec
                  (for [row (range (/ height tileheight))]
                    (t/crop entity
                      (* col tilewidth)
                      (* row tileheight)
                      tilewidth
                      tileheight)))))))))))
            

