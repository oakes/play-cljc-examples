(ns dungeon-crawler.entities
  (:require [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [dungeon-crawler.move :as move]
            [dungeon-crawler.tiles :as tiles]))

(defrecord Entity [id
                   char-type
                   moves
                   attacks
                   specials
                   hits
                   deads
                   direction
                   current-image
                   width
                   height
                   x
                   y
                   x-change
                   y-change
                   x-velocity
                   y-velocity
                   game
                   last-attack])

(def tile-size 256)
(def *latest-id (atom 0))
(def player-spawn-point {:x 2.5 :y 2.5})
(def spawn-points (for [row (range tiles/rows)
                        col (range tiles/cols)
                        :let [point {:x (-> row (* 10) (+ 2.5))
                                     :y (-> col (* 10) (+ 2.5))}]
                        :when (not= point player-spawn-point)]
                    point))
;; the entities are cached here so we don't constantly
;; make new ones when reloading the core namespace
(defonce *entity-cache (atom {}))

(def spawn-data
 [{:char-type :player
   :path "characters/male_light.png"
   :mask-size 128
   :instances [player-spawn-point]}
  {:char-type :ogre
   :path "characters/ogre.png"
   :mask-size 256
   :instances (->> spawn-points shuffle (take 5))}
  {:char-type :elemental
   :path "characters/elemental.png"
   :mask-size 256
   :instances (->> spawn-points shuffle (take 5))}]) 

(defn create-grid [image tile-size mask-size]
  (let [offset (-> tile-size (- mask-size) (/ 2))]
    (vec (for [y (range 0 (:height image) tile-size)]
           (vec (for [x (range 0 (:width image) tile-size)]
                  (t/crop image (+ x offset) (+ y offset) mask-size mask-size)))))))

(defn ->entity' [entity char-type mask-size x y]
  (let [grid (create-grid entity tile-size mask-size)
        moves (zipmap move/directions
                (map #(vec (take 4 %)) grid))
        attacks (zipmap move/directions
                  (map #(nth % 4) grid))
        specials (zipmap move/directions
                   (map #(nth % 5) grid))
        hits (zipmap move/directions
               (map #(nth % 6) grid))
        deads (zipmap move/directions
                (map #(nth % 7) grid))
        [x y] (tiles/isometric->screen x y)]
    (map->Entity
      {:id (swap! *latest-id inc)
       :char-type char-type
       :moves moves
       :attacks attacks
       :specials specials
       :hits hits
       :deads deads
       :direction :s
       :current-image (get-in moves [:s 0])
       :width (/ mask-size tile-size)
       :height (/ mask-size tile-size)
       :x x
       :y y
       :x-change 0
       :y-change 0
       :x-velocity 0
       :y-velocity 0
       :last-attack 0})))

(defn ->entity [game {:keys [char-type mask-size]} {:keys [data width height]} {:keys [x y]}]
  (-> (or (char-type @*entity-cache)
          (->> (e/->image-entity game data width height)
               (c/compile game)
               (swap! *entity-cache assoc char-type)
               char-type))
      (->entity' char-type mask-size x y)))


