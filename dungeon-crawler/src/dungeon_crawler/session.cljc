(ns dungeon-crawler.session
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.move :as move]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [clara.rules :as clara]
            [clarax.rules :as clarax]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]]))
  #?(:cljs (:require-macros [dungeon-crawler.session :refer [->session-wrapper]])))

(def orig-camera (e/->camera true))
(def vertical-tiles 7)
(def max-cursor-distance 0.5)
(def animation-duration 0.5)
(def restart-delay 1)

(defrecord Entity [id
                   char-type
                   moves
                   attacks
                   specials
                   hits
                   deads
                   direction
                   animate?
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
                   last-attack
                   health
                   damage
                   attack-delay])
(defrecord Game [total-time delta-time context])
(defrecord Window [width height])
(defrecord Camera [camera window player min-y max-y])
(defrecord Mouse [x y world-coords button])
(defrecord Keys [pressed])
(defrecord TiledMap [layers width height entities])
(defrecord Attack [source-id target-id])
(defrecord Animation [entity-id type expire-time])
(defrecord Direction [entity-id x y])
(defrecord Sound [file-name])
(defrecord Damage [entity-id damage])
(defrecord Restart [restart-time])

(defn update-camera [window player]
  (let [{game-width :width game-height :height} window
        scaled-tile-size (/ game-height vertical-tiles)
        x (:x player)
        y (:y player)
        offset-x (/ game-width 2 scaled-tile-size)
        offset-y (/ game-height 2 scaled-tile-size)
        min-y (- y offset-y 1)
        max-y (+ y offset-y)]
    {:window window
     :camera (t/translate orig-camera (- x offset-x) (- y offset-y))
     :player player
     :min-y min-y
     :max-y max-y}))

(defn update-mouse [window mouse player]
  (let [{:keys [x y]} mouse
        {:keys [width height]} window
        ;; convert mouse coords to (-1 to 1) coords
        matrix (->> (m/projection-matrix width height)
                    (m/multiply-matrices 3 (m/translation-matrix x y)))
        wx (nth matrix 6)
        wy (* -1 (nth matrix 7))
        ;; convert to tile coords
        y-multiplier (/ vertical-tiles 2)
        x-multiplier (* y-multiplier (/ width height))
        wx (* wx x-multiplier)
        wy (* wy y-multiplier)
        ;; make mouse relative to player position
        wx (-> wx
               (+ (:x player))
               (- (:width player)))
        wy (-> wy
               (+ (:y player))
               (- (:height player)))]
   {:world-coords {:x wx :y wy}}))

(def queries
  '{:get-game
    (fn []
      (let [game Game]
        game))
    :get-window
    (fn []
      (let [window Window]
        window))
    :get-camera
    (fn []
      (let [camera Camera]
        camera))
    :get-mouse
    (fn []
      (let [mouse Mouse]
        mouse))
    :get-keys
    (fn []
      (let [keys Keys]
        keys))
    :get-player
    (fn []
      (let [entity Entity
            :when (= (:char-type entity) :player)]
        entity))
    :get-enemies
    (fn []
      (let [entity [Entity]
            :when (not= (:char-type entity) :player)]
        entity))
    :get-tiled-map
    (fn []
      (let [tiled-map TiledMap]
        tiled-map))
    :get-enemy-under-cursor
    (fn []
      (let [mouse Mouse
            :when (not= nil (:world-coords mouse))
            target [Entity]
            :when (not= (:char-type target) :player)]
        (some->> target
                 (mapv #(vector (move/calc-distance % (:world-coords mouse)) %))
                 (sort-by first)
                 (filter #(<= (first %) max-cursor-distance))
                 first
                 second)))
    :should-restart?
    (fn []
      (let [game Game
            restart Restart]
        (some-> (:restart-time restart)
                (<= (:total-time game)))))})

(def rules
  '{:move-enemy
    (let [game Game
          player Entity
          :when (= (:char-type player) :player)
          entity Entity
          :when (and (not= (:game entity) game)
                     (not= (:char-type entity) :player)
                     (> (:health entity) 0))]
      (clarax/merge! entity (-> (move/get-enemy-velocity entity player)
                                (move/move entity game)
                                (assoc :game game :animate? true))))
    :move-player
    (let [game Game
          window Window
          keys Keys
          mouse Mouse
          player Entity
          :when (and (not= (:game player) game)
                     (= (:char-type player) :player)
                     (> (:health player) 0))]
      (clarax/merge! player (-> (move/get-player-velocity window (:pressed keys) mouse player)
                                (move/move player game)
                                (assoc :game game :animate? true))))
    :update-camera
    (let [window Window
          :when (or (pos? (:width window))
                    (pos? (:height window)))
          player Entity
          :when (= (:char-type player) :player)
          camera Camera
          :when (or (not= (:window camera) window)
                    (not= (:player camera) player))]
      (clarax/merge! camera (update-camera window player)))
    :animate
    (let [game Game
          entity Entity
          :when (= true (:animate? entity))
          animation [Animation]
          :when (= (:id entity) (:entity-id animation))]
      (->> (move/animate entity game animation)
           (merge {:animate? false})
           (clarax/merge! entity)))
    :dont-overlap-tile
    (let [tiled-map TiledMap
          entity Entity
          :when (or (not= 0 (:x-change entity))
                    (not= 0 (:y-change entity)))]
      (some->> (move/dont-overlap-tile entity tiled-map)
               (clarax/merge! entity)))
    :player-attack-with-key
    (let [game Game
          player Entity
          :when (and (= (:char-type player) :player)
                     (-> (:total-time game)
                         (- (:last-attack player))
                         (>= (:attack-delay player))))
          keys Keys
          :when (contains? (:pressed keys) :space)
          target [Entity]
          :when (and (not= (:char-type target) :player)
                     (> (:health target) 0))]
      (clarax/merge! player {:last-attack (:total-time game)})
      (when-let [target (some->> target
                                 (mapv #(vector (move/calc-distance % player) %))
                                 (sort-by first)
                                 (filter #(<= (first %) move/max-attack-distance))
                                 first
                                 second)]
        (clara/insert-unconditional! (->Attack (:id player) (:id target)))
        (clara/insert-unconditional! (->Direction (:id player) (:x target) (:y target)))))
    :player-attack-with-mouse
    (let [game Game
          player Entity
          :when (and (= (:char-type player) :player)
                     (-> (:total-time game)
                         (- (:last-attack player))
                         (>= (:attack-delay player))))
          mouse Mouse
          :when (and (= (:button mouse) :right)
                     (not= nil (:world-coords mouse)))
          target [Entity]
          :when (and (not= (:char-type target) :player)
                     (> (:health target) 0))]
      (clarax/merge! player {:last-attack (:total-time game)})
      (when-let [target (some->> target
                                 (mapv #(vector (move/calc-distance % (:world-coords mouse)) %))
                                 (sort-by first)
                                 (filter #(<= (first %) max-cursor-distance))
                                 first
                                 second)]
        (clara/insert-unconditional! (->Attack (:id player) (:id target)))
        (let [{:keys [x y]} (:world-coords mouse)]
          (clara/insert-unconditional! (->Direction (:id player) x y)))))
    :enemy-attack
    (let [game Game
          entity Entity
          :when (and (not= (:char-type entity) :player)
                     (> (:health entity) 0)
                     (-> (:total-time game)
                         (- (:last-attack entity))
                         (>= (:attack-delay entity))))
          player Entity
          :when (and (= (:char-type player) :player)
                     (<= (move/calc-distance entity player) move/max-attack-distance)
                     (> (:health player) 0))]
      (clarax/merge! entity {:last-attack (:total-time game)})
      (clara/insert-unconditional! (->Attack (:id entity) (:id player))))
    :update-mouse-world-coords
    (let [window Window
          mouse Mouse
          :when (= nil (:world-coords mouse))
          player Entity
          :when (= (:char-type player) :player)]
      (clarax/merge! mouse (update-mouse window mouse player)))
    :attack
    (let [game Game
          attack Attack
          source Entity
          :when (= (:id source) (:source-id attack))
          target Entity
          :when (= (:id target) (:target-id attack))]
      (clara/retract! attack)
      (when (<= (move/calc-distance source target)
                move/max-attack-distance)
        (let [duration (+ (:total-time game) animation-duration)
              sound-file (if (= (:char-type source) :player)
                           "monsterhurt.wav"
                           "playerhurt.wav")]
          (clara/insert-unconditional! (->Sound sound-file))
          (->> duration
               (->Animation (:id source) :attacks)
               clara/insert-unconditional!)
          (->> duration
               (->Animation (:id target) :hits)
               clara/insert-unconditional!)
          (->> (:damage source)
               (->Damage (:id target))
               clara/insert-unconditional!))))
    :damage
    (let [game Game
          damage Damage
          entity Entity
          :when (= (:id entity) (:entity-id damage))]
      (clara/retract! damage)
      (let [health (- (:health entity) (:damage damage))]
        (clarax/merge! entity {:health health :animate? true})
        (when (<= health 0)
          (clara/insert-unconditional! (->Sound "death.wav"))
          (when (= (:char-type entity) :player)
            (clara/insert-unconditional! (->Restart (+ (:total-time game) restart-delay)))))))
    :remove-expired-animations
    (let [game Game
          animation Animation
          :when (<= (:expire-time animation) (:total-time game))]
      (clara/retract! animation))
    :change-direction
    (let [direction Direction
          entity Entity
          :when (= (:id entity) (:entity-id direction))]
      (clara/retract! direction)
      (->> (move/get-direction
             (- (:x direction) (:x entity))
             (- (:y direction) (:y entity)))
           (hash-map :direction)
           (clarax/merge! entity)))
    :play-sound
    (let [sound Sound]
      (clara/retract! sound)
      (utils/play-sound! (:file-name sound)))})

#?(:clj (defmacro ->session-wrapper []
          (list '->session (merge queries rules))))

(def *session (atom nil))

(defn restart! []
  (reset! *session
    (-> (->session-wrapper)
        (clara/insert
          (->Mouse 0 0 nil nil)
          (->Keys #{}))
        clara/fire-rules)))

(restart!)

;; this is a perf optimization.
;; while we could call clara/query with these keywords directly,
;; saving them to global vars will avoid the hash lookup inside the game loop.
(let [query-fns (clarax/query-fns @*session)]
  (def get-game (:get-game query-fns))
  (def should-restart? (:should-restart? query-fns))
  (def get-player (:get-player query-fns))
  (def get-enemies (:get-enemies query-fns))
  (def get-tiled-map (:get-tiled-map query-fns))
  (def get-window (:get-window query-fns))
  (def get-camera (:get-camera query-fns))
  (def get-keys (:get-keys query-fns))
  (def get-mouse (:get-mouse query-fns))
  (def get-enemy-under-cursor (:get-enemy-under-cursor query-fns)))

