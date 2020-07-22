(ns dungeon-crawler.tiles
  (:require [dungeon-crawler.utils :as utils]
            [clojure.set :as set]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.instances :as i]
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

;; http://clintbellanger.net/articles/isometric_math/

(def ^:const tile-width-half (/ 1 2))
(def ^:const tile-height-half (/ 1 4))

(defn isometric->screen [x y]
  [(* (- x y) tile-width-half)
   (* (+ x y) tile-height-half)])

(defn screen->isometric [x y]
  [(/ (+ (/ x tile-width-half)
         (/ y tile-height-half))
      2)
   (/ (- (/ y tile-height-half)
         (/ x tile-width-half))
      2)])

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
        {{:keys [tilewidth tileheight]} :attrs} tileset
        layers (->> parsed :content
                    (filter #(= :layer (:tag %)))
                    (map #(vector
                            (-> % :attrs :name)
                            (-> % :content first :content first)))
                    (into {}))]
    (utils/get-image (-> image :attrs :source)
      (fn [{:keys [data width height]}]
        (let [entity-width (* tilewidth map-width)
              entity-height (* tileheight map-height)
              entity (e/->image-entity game data width height)
              tiles-vert (/ height tileheight)
              tiles-horiz (/ width tilewidth)
              images (vec
                       (for [y (range tiles-vert)
                             x (range tiles-horiz)]
                         (t/crop entity
                                 (* x tilewidth)
                                 (* y tileheight)
                                 tilewidth
                                 tileheight)))
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
                       partitioned-layers)
              entity (c/compile game (i/->instanced-entity entity))
              entities (->> (for [layer ["walls"]
                                  i (range (count (get layers layer)))
                                  :let [x (mod i map-width)
                                        y (int (/ i map-width))
                                        id (dec (nth (get layers layer) i))]
                                  :when (>= id 0)]
                              (let [image (nth images id)
                                    [x y] (isometric->screen x y)
                                    x (- x (/ 1 2))
                                    y (- y 1)]
                                [y (t/translate image x y)]))
                            (group-by first)
                            (mapv
                              (fn [[y tiles]]
                                (let [tiles (mapv second tiles)]
                                  [y (reduce-kv i/assoc entity tiles)]))))]
          (callback
            {::layers partitioned-layers
             ::width map-width
             ::height map-height
             ::entities entities}))))))

(defn touching-tile? [layers layer-name x y width height]
  (let [[x y] (screen->isometric
                (+ x (/ width 2))
                (+ y height))
        layer (get layers layer-name)
        start-x (math round (float x))
        start-y (math round (float y))
        end-x (math round (float (+ start-x width)))
        end-y (math round (float (+ start-y height)))
        tiles (for [tile-x (range start-x (inc end-x))
                    tile-y (range start-y (inc end-y))]
                (get-in layer [tile-y tile-x]))]
    (some? (first (filter pos? (remove nil? tiles))))))

