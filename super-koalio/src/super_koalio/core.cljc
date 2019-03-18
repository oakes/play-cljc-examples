(ns super-koalio.core
  (:require [super-koalio.utils :as utils]
            [super-koalio.move :as move]
            [tile-soup.core :as ts]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj  [super-koalio.tile :as tile :refer [read-tiled-map]]
               :cljs [super-koalio.tile :as tile :refer-macros [read-tiled-map]])))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}
                       :x-velocity 0
                       :y-velocity 0
                       :player-x 0
                       :player-y 0
                       :can-jump? false
                       :direction :right
                       :player-images {}
                       :player-walk-keys [:walk1 :walk2 :walk3]
                       :player-image-key :jump
                       :tiled-map-images nil}))

(def tiled-xml (read-tiled-map "level1.tmx"))
(def tiled-map (ts/parse tiled-xml))
(def map-width (-> tiled-map :attrs :width))
(def map-height (-> tiled-map :attrs :height))

(def map-layers (->> tiled-map :content
                     (filter #(= :layer (:tag %)))
                     (map #(vector
                             (-> % :attrs :name)
                             (-> % :content first :content first)))
                     (into {})))

(def koala-width 18)
(def koala-height 26)

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load koalio image
  (utils/get-image "koalio.png"
    (fn [{:keys [data width height]}]
      (let [entity (c/compile game (e/->image-entity game data width height))
            images (vec (for [i (range 5)]
                          (t/crop entity
                             (* i koala-width)
                             0
                             koala-width
                             koala-height)))
            [stand jump walk1 walk2 walk3] images]
        ;; add it to the state
        (swap! *state update :player-images assoc
          :stand stand
          :jump jump
          :walk1 walk1
          :walk2 walk2
          :walk3 walk3))))
  ;; load the tiled map
  (tile/load-tiled-map game tiled-map *state))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(def transform-tile
  (memoize
    (fn [tile x y game-width game-height tile-size]
      (-> tile
          (t/project game-width game-height)
          (t/translate (* x tile-size) (* y tile-size))
          (t/scale tile-size tile-size)))))

(defn run [game]
  (let [{:keys [entities
                pressed-keys
                player-x
                player-y
                direction
                player-images
                player-image-key
                tiled-map-images]
         :as state} @*state
        game-width (utils/get-width game)
        game-height (utils/get-height game)
        tile-size (/ game-height map-height)]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; render the tiled map
    (doseq [layer ["background" "walls"]]
      (when-let [walls (get map-layers layer)]
        (doseq [i (range (count walls))
                :let [x (mod i map-width)
                      y (int (/ i map-width))
                      id (dec (nth walls i))]
                :when (and (>= id 0)
                           (<= x (* map-width tile-size)))]
          (let [image (nth tiled-map-images id)]
            (c/render game (transform-tile image x y game-width game-height tile-size))))))
    ;; get the current player image to display
    (when-let [player (get player-images player-image-key)]
      (let [player-width  tile-size
            player-height (* player-width (/ koala-height koala-width))]
        ;; render the player
        (c/render game
          (-> player
              (t/project game-width game-height)
              (t/translate (cond-> player-x
                                   (= direction :left)
                                   (+ player-width))
                player-y)
              (t/scale (cond-> player-width
                               (= direction :left)
                               (* -1))
                player-height)))
        ;; change the state to move the player
        (swap! *state
          (fn [state]
            (->> (assoc state
                   :player-width player-width
                   :player-height player-height)
                 (move/move game)
                 (move/prevent-move game)
                 (move/animate game)))))))
  ;; return the game map
  game)

