(ns dungeon-crawler.tile
  (:require [dungeon-crawler.utils :as utils]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            #?@(:clj [[clojure.java.io :as io]
                      [tile-soup.core :as ts]])
            #?(:clj  [play-cljc.macros-java :refer [math]]
               :cljs [play-cljc.macros-js :refer-macros [math]])))

#?(:clj (defmacro read-tiled-map [fname]
          (-> (str "public/" fname)
              io/resource
              slurp
              ts/parse
              pr-str)))

(defn isometric->screen [x y]
  [(- x y)
   (/ (+ x y) 2)])

(defn transform-tile [tile x y width height tile-width tile-height]
  (let [[x y] (isometric->screen x y)
        x (* x (/ tile-width 2))
        y (* y (/ tile-height 2))]
    (-> tile
        (t/project width height)
        (t/translate
          (+ x (/ width 2) (- (/ tile-width 2)))
          y)
        (t/scale tile-width tile-height))))

(def flip-y-matrix
  [1  0  0
   0 -1  0
   0  0  1])

(defn load-tiled-map [game parsed callback]
  (let [map-width (-> parsed :attrs :width)
        map-height (-> parsed :attrs :height)
        tileset (first (filter #(= :tileset (:tag %)) (:content parsed)))
        image (first (filter #(= :image (:tag %)) (:content tileset)))
        tile-width (-> tileset :attrs :tilewidth)
        tile-height (-> tileset :attrs :tileheight)
        layers (->> parsed :content
                    (filter #(= :layer (:tag %)))
                    (map #(vector
                            (-> % :attrs :name)
                            {:tiles (-> % :content first :content first)
                             :width (-> % :attrs :width)
                             :height (-> % :attrs :height)}))
                    (into {}))]
    (utils/get-image (-> image :attrs :source)
      (fn [{:keys [data width height]}]
        (let [entity-width (* tile-width map-width)
              entity-height (* tile-height map-height)
              iso-tile-width 64
              iso-tile-height 32
              iso-entity-width (* iso-tile-width map-width)
              iso-entity-height (* iso-tile-height map-width)
              outer-entity (e/->image-entity game nil entity-width entity-height)
              inner-entity (c/compile game (-> (e/->image-entity game data width height)
                                               (assoc :viewport {:x 0 :y 0 :width entity-width :height entity-height})))
              tiles-vert (/ height tile-height)
              tiles-horiz (/ width tile-width)
              images (vec
                       (for [y (range tiles-vert)
                             x (range tiles-horiz)]
                         (t/crop inner-entity
                           (* x tile-width)
                           (* y tile-height)
                           tile-width
                           tile-height)))
              partitioned-layers (reduce-kv
                                   (fn [m k {:keys [tiles]}]
                                     (assoc m k (->> tiles
                                                     (partition map-width)
                                                     (mapv vec))))
                                   {}
                                   layers)]
          (callback
            {:layers partitioned-layers
             :map-width map-width
             :map-height map-height}
            (update-in
              (c/compile game
                (assoc outer-entity
                  :width iso-entity-width
                  :height iso-entity-height
                  :render-to-texture
                  {'u_image
                   (->> (for [layer ["walls"]
                              i (range (count (get-in layers [layer :tiles])))
                              :let [x (mod i map-width)
                                    y (int (/ i map-width))
                                    id (dec (nth (get-in layers [layer :tiles]) i))]
                              :when (>= id 0)]
                          (let [image (nth images id)]
                            (transform-tile image x y iso-entity-width iso-entity-height iso-tile-width iso-tile-height)))
                        (sort-by #(get-in % [:uniforms 'u_matrix 7]) >)
                        vec)}))
              [:uniforms 'u_matrix]
              #(m/multiply-matrices 3 flip-y-matrix %))))))))

(defn touching-tile? [{:keys [layers]} layer-name x y width height]
  (let [layer (get layers layer-name)
        start-x (int x)
        start-y (int y)
        end-x (inc (int (+ x width)))
        end-y (int (+ y height))
        tiles (for [tile-x (range start-x end-x)
                    tile-y (range end-y start-y -1)]
                (get-in layer [tile-y tile-x]))]
    (some? (first (filter pos? (remove nil? tiles))))))

