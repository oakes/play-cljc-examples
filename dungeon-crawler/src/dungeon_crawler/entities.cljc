(ns dungeon-crawler.entities
  (:require [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [dungeon-crawler.tiles :as tiles]))

(def directions [:w :nw :n :ne
                 :e :se :s :sw])
(def velocities [[-1 0] [-1 -1] [0 -1] [1 -1]
                 [1 0] [1 1] [0 1] [-1 1]])
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
 [{:attrs {::kind :player
           ::health 20
           ::damage 4
           ::attack-delay 0.25}
   :path "characters/male_light.png"
   :mask-size 128
   :instances [player-spawn-point]}
  {:attrs {::kind :ogre
           ::health 8
           ::damage 2
           ::attack-delay 1}
   :path "characters/ogre.png"
   :mask-size 256
   :instances (->> spawn-points shuffle (take 5))}
  {:attrs {::kind :elemental
           ::health 6
           ::damage 1
           ::attack-delay 0.75}
   :path "characters/elemental.png"
   :mask-size 256
   :instances (->> spawn-points shuffle (take 5))}]) 

(defn create-grid [image tile-size mask-size]
  (let [offset (-> tile-size (- mask-size) (/ 2))]
    (vec (for [y (range 0 (:height image) tile-size)]
           (vec (for [x (range 0 (:width image) tile-size)]
                  (t/crop image (+ x offset) (+ y offset) mask-size mask-size)))))))

(defn ->entity' [entity attrs mask-size x y]
  (let [grid (create-grid entity tile-size mask-size)
        moves (zipmap directions
                (map #(vec (take 4 %)) grid))
        attacks (zipmap directions
                  (map #(nth % 4) grid))
        specials (zipmap directions
                   (map #(nth % 5) grid))
        hits (zipmap directions
               (map #(nth % 6) grid))
        deads (zipmap directions
                (map #(nth % 7) grid))
        [x y] (tiles/isometric->screen x y)]
    (merge attrs
      {::moves moves
       ::attacks attacks
       ::specials specials
       ::hits hits
       ::deads deads
       ::direction :s
       ::current-image (get-in moves [:s 0])
       ::width (/ mask-size tile-size)
       ::height (/ mask-size tile-size)
       ::x x
       ::y y
       ::x-change 0
       ::y-change 0
       ::x-velocity 0
       ::y-velocity 0
       ::last-attack 0
       ::current-animation :none
       ::animation-expiration 0})))

(defn ->entity [game {:keys [attrs mask-size]} {:keys [data width height]} {:keys [x y]}]
  (let [{:keys [::kind]} attrs]
    (-> (or (kind @*entity-cache)
            (->> (e/->image-entity game data width height)
                 (c/compile game)
                 (swap! *entity-cache assoc kind)
                 kind))
        (->entity' attrs mask-size x y))))

