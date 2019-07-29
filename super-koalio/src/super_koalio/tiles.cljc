(ns super-koalio.tiles
  (:require [super-koalio.utils :as utils]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.gl.entities-instanced :as ei]
            [play-cljc.gl.tiles :as tiles]
            #?@(:clj [[clojure.java.io :as io]
                      [tile-soup.core :as ts]])))

#?(:clj (defmacro read-tiled-map [fname]
          (-> (str "public/" fname)
              io/resource
              slurp
              ts/parse
              pr-str)))

(defn load-tiled-map [game parsed callback]
  (let [map-width (-> parsed :attrs :width)
        map-height (-> parsed :attrs :height)
        tileset (first (filter #(= :tileset (:tag %)) (:content parsed)))
        image (first (filter #(= :image (:tag %)) (:content tileset)))
        {{:keys [tilewidth tileheight]} :attrs} tileset
        layers (->> parsed :content
                    (filter #(= :layer (:tag %)))
                    (map #(vector
                            (-> % :attrs :name)
                            (-> % :content first :content first)))
                    (into {}))]
    (utils/get-image (-> image :attrs :source)
      (fn [{:keys [data width height]}]
        (let [entity (tiles/->tile-entity (e/->image-entity game data width height) tilewidth tileheight)
              tiles-vert (/ height tileheight)
              tiles-horiz (/ width tilewidth)
              images (vec
                       (for [y (range tiles-vert)
                             x (range tiles-horiz)]
                         (t/crop entity x y 1 1)))
              tiles (vec
                      (for [layer ["background" "walls"]
                            i (range (count (get layers layer)))
                            :let [x (mod i map-width)
                                  y (int (/ i map-width))
                                  id (dec (nth (get layers layer) i))]
                            :when (>= id 0)]
                        (t/translate (nth images id) x y)))
              entity (ei/->instanced-entity entity (count tiles))
              entity (reduce-kv ei/assoc entity tiles)
              entity (c/compile game entity)
              partitioned-layers (reduce-kv
                                   (fn [m k v]
                                     (assoc m k (->> v
                                                     (partition map-width)
                                                     (mapv vec))))
                                   {}
                                   layers)]
          (callback
            {:layers partitioned-layers
             :map-width map-width
             :map-height map-height}
            entity))))))

(defn touching-tile? [{:keys [layers]} layer-name x y width height]
  (let [layer (get layers layer-name)
        start-x (int x)
        start-y (int y)
        end-x (int (+ x width))
        end-y (int (+ y height))
        tiles (for [tile-x (range start-x (inc end-x))
                    tile-y (range start-y (inc end-y))]
                (get-in layer [tile-y tile-x]))]
    (some? (first (filter pos? (remove nil? tiles))))))

