(ns dungeon-crawler.tile
  (:require [dungeon-crawler.utils :as utils]
            [clojure.set :as set]
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

(defn screen->isometric [x y]
  (let [y (- y (/ x 2))]
    [(+ x y) y]))

(def flip-y-matrix
  [1  0  0
   0 -1  0
   0  0  1])

(defn transform-tile [tile x y width height tile-width tile-height]
  (let [[x y] (isometric->screen x y)
        x (* x (/ tile-width 2))
        y (* y (/ tile-height 2))]
    (-> tile
        (t/project width height)
        (t/translate
          (+ x (/ width 2) (- (/ tile-width 2)))
          (+ y tile-height))
        (t/scale tile-width tile-height)
        (update-in [:uniforms 'u_matrix] #(m/multiply-matrices 3 flip-y-matrix %)))))

(def ^:const cols 4)
(def ^:const rows 4)
(def ^:const size 10)

(defn get-rand-neighbor
  [rooms {:keys [x y] :as room}]
  (let [res (->> #{(assoc room :x (- x 1))
                   (assoc room :y (- y 1))
                   (assoc room :x (+ x 1))
                   (assoc room :y (+ y 1))}
                 (set/intersection (set rooms))
                 vec)]
    (when (seq res)
      (rand-nth res))))

(defn connect-room [layers r1 r2]
  (let [rand-spot (+ 1 (rand-int (- size 3)))
        x-diff (- (:x r2) (:x r1))
        y-diff (- (:y r2) (:y r1))]
    (reduce
      (fn [layers i]
        (let [x (+ (* (:x r1) size)
                   rand-spot
                   (* x-diff i))
              y (+ (* (:y r1) size)
                   rand-spot
                   (* y-diff i))]
          (-> layers
              (assoc-in ["walls" x y] 0)
              (assoc-in ["walls" (inc x) y] 0)
              (assoc-in ["walls" x (inc y)] 0)
              (assoc-in ["walls" (inc x) (inc y)] 0))))
      layers
      (range size))))

(defn connect-rooms [layers rooms room]
  (let [visited-room (assoc room :visited? true)
        rooms (mapv #(if (= % room) visited-room %) rooms)]
    (if-let [next-room (get-rand-neighbor rooms room)]
      (let [layers (connect-room layers room next-room)]
        (loop [layers layers
               rooms rooms]
          (let [[layers new-rooms] (connect-rooms layers rooms next-room)]
            (if (= rooms new-rooms)
              [layers rooms]
              (recur layers new-rooms)))))
      [layers rooms])))

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
                            (-> % :content first :content first)))
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
                                   (fn [m k tiles]
                                     (assoc m k (->> tiles
                                                     (partition map-width)
                                                     (mapv vec))))
                                   {}
                                   layers)
              rooms (for [row (range rows)
                          col (range cols)]
                      {:x row :y col})
              [partitioned-layers] (connect-rooms partitioned-layers rooms {:x 0 :y 0})
              layers (reduce-kv
                       (fn [m k tiles]
                         (assoc m k (->> tiles
                                         (apply concat)
                                         vec)))
                       {}
                       partitioned-layers)]
          (callback
            {:layers partitioned-layers
             :map-width map-width
             :map-height map-height}
            (c/compile game
              (assoc outer-entity
                :width iso-entity-width
                :height iso-entity-height
                :render-to-texture
                {'u_image
                 (->> (for [layer ["walls"]
                            i (range (count (get layers layer)))
                            :let [x (mod i map-width)
                                  y (int (/ i map-width))
                                  id (dec (nth (get layers layer) i))]
                            :when (>= id 0)]
                        (let [image (nth images id)]
                          (transform-tile image x y iso-entity-width iso-entity-height iso-tile-width iso-tile-height)))
                      (sort-by #(get-in % [:uniforms 'u_matrix 7]) <)
                      vec)}))))))))

(defn touching-tile? [{:keys [layers map-width map-height]} layer-name x y width height]
  (let [[x y] (screen->isometric
                (+ x (/ (- 1 width) 2))
                (+ y height))
        layer (get layers layer-name)
        start-x (math round x)
        start-y (math round y)
        end-x (math round (+ start-x width))
        end-y (math round (+ start-y height))
        tiles (for [tile-x (range start-x (inc end-x))
                    tile-y (range start-y (inc end-y))]
                (get-in layer [(- map-width tile-x) (- map-height tile-y)]))]
    (some? (first (filter pos? (remove nil? tiles))))))

