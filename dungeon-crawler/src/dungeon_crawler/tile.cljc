(ns dungeon-crawler.tile
  (:require [dungeon-crawler.utils :as utils]
            [tile-soup.core :as ts]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            #?(:clj [clojure.java.io :as io])))

#?(:clj (defmacro read-tiled-map [fname]
          (slurp (io/resource (str "public/" fname)))))

(defn transform-tile [tile x y width height tile-width tile-height]
  (let [x (* x (/ tile-width 2))
        y (* y (/ tile-height 2))]
    (-> tile
        (t/project width height)
        (t/translate
          (+ x y)
          (+ (/ (- y x) 2)
             (/ height 2)))
        (t/scale tile-width tile-height))))

(def flip-y-matrix
  [1  0  0
   0 -1  0
   0  0  1])

(defn load-tiled-map [game tiled-xml callback]
  (let [parsed (ts/parse tiled-xml)
        map-width (-> parsed :attrs :width)
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
        (let [tile-width (* 3 tilewidth)
              tile-height (* 3 tileheight)
              entity-width (* tile-width map-width)
              entity-height (* tile-height map-height)
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
             :tile-width tile-width
             :tile-height tile-height}
            (update-in
              (c/compile game
                (assoc outer-entity
                  :width entity-width
                  :height entity-height
                  :render-to-texture
                  {'u_image
                   (->> (for [layer ["walls"]
                              i (range (count (get layers layer)))
                              :let [x (mod i map-width)
                                    y (int (/ i map-width))
                                    id (dec (nth (get layers layer) i))]
                              :when (>= id 0)]
                          (let [image (nth images id)]
                            (transform-tile image x y entity-width entity-height tile-width tile-height)))
                        (sort-by #(get-in % [:uniforms 'u_matrix 7]) >)
                        vec)}))
              [:uniforms 'u_matrix]
              #(m/multiply-matrices 3 flip-y-matrix %))))))))

(defn touching-tile? [{:keys [layers map-width map-height tile-width tile-height]}
                      layer-name game-height x y width height]
  (let [layer (get layers layer-name)
        ratio (/ (* map-height tile-height)
                 game-height)
        [x y width height] (mapv #(* % ratio) [x y width height])
        start-x (int (/ x tile-width))
        start-y (int (/ y tile-height))
        end-x (inc (int (/ (+ x width) tile-width)))
        end-y (int (/ (+ y height) tile-height))
        tiles (for [tile-x (range start-x end-x)
                    tile-y (range end-y start-y -1)]
                (get-in layer [tile-y tile-x]))]
    (some? (first (filter pos? (remove nil? tiles))))))

