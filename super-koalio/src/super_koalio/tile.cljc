(ns super-koalio.tile
  (:require [super-koalio.utils :as utils]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            #?@(:clj [[clojure.java.io :as io]
                      [tile-soup.core :as ts]])))

#?(:clj (defmacro read-tiled-map [fname]
          (-> (str "public/" fname)
              io/resource
              slurp
              ts/parse
              pr-str)))

(defn transform-tile [tile x y width height tile-size]
  (-> tile
      (t/project width height)
      (t/translate (* x tile-size) (* y tile-size))
      (t/scale tile-size tile-size)))

(def flip-y-matrix
  [1  0  0
   0 -1  0
   0  0  1])

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
        (let [tile-size (/ height map-height)
              entity-width (* tile-size map-width)
              entity-height (* tile-size map-height)
              outer-entity (e/->image-entity game nil entity-width entity-height)
              inner-entity (c/compile game (-> (e/->image-entity game data width height)
                                               (assoc :viewport {:x 0 :y 0 :width entity-width :height entity-height})))
              tiles-vert (/ height tileheight)
              tiles-horiz (/ width tilewidth)
              images (vec
                       (for [y (range tiles-vert)
                             x (range tiles-horiz)]
                         (t/crop inner-entity
                           (* x tilewidth)
                           (* y tileheight)
                           tilewidth
                           tileheight)))
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
             :map-height map-height
             :tile-width tilewidth
             :tile-height tileheight}
            (update-in
              (c/compile game
                (assoc outer-entity
                  :width entity-width
                  :height entity-height
                  :render-to-texture
                  {'u_image
                   (vec (for [layer ["background" "walls"]
                              i (range (count (get layers layer)))
                              :let [x (mod i map-width)
                                    y (int (/ i map-width))
                                    id (dec (nth (get layers layer) i))]
                              :when (>= id 0)]
                          (let [image (nth images id)]
                            (transform-tile image x y entity-width entity-height tile-size))))}))
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

