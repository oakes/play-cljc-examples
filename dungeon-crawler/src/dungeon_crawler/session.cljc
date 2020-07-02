(ns dungeon-crawler.session
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.move :as move]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [clara.rules :as clara]
            [clara.rules.accumulators :as acc]
            [clarax.rules :as clarax]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]]))
  #?(:cljs (:require-macros [dungeon-crawler.session :refer [merge-into-session]])))

(def orig-camera (e/->camera))
(def vertical-tiles 7)
(def max-cursor-distance 0.5)
(def animation-duration 0.5)

(defrecord Entity [id
                   kind
                   moves
                   attacks
                   specials
                   hits
                   deads
                   x
                   y
                   x-change
                   y-change
                   x-velocity
                   y-velocity
                   game-anchor])
(defrecord Size [id width height])
(defrecord Direction [id value]) ;; :n, :s, ...
(defrecord CurrentImage [id value]) ;; an image entity
(defrecord DistanceFromCursor [id value]) ;; number - how far is this entity from the cursor
(defrecord DistanceFromPlayer [id value]) ;; number - how far is this entity from the player
(defrecord Health [id value]) ;; number
(defrecord Damage [id value]) ;; number - how much damage this entity deals per attack
(defrecord LastAttack [id value]) ;; number - timestamp of the last attack
(defrecord AttackDelay [id value]) ;; number - minimum time between attacks

(defrecord Game [total-time delta-time context])
(defrecord Window [width height])
(defrecord Camera [camera window-anchor player-anchor min-y max-y])
(defrecord Mouse [x y world-coords button])
(defrecord Keys [pressed])
(defrecord TiledMap [layers width height entities])
(defrecord Attack [source-id target-id])
(defrecord Animation [entity-id kind expire-time])

(defn update-camera [window player]
  (let [{game-width :width game-height :height} window
        scaled-tile-size (/ game-height vertical-tiles)
        x (:x player)
        y (:y player)
        offset-x (/ game-width 2 scaled-tile-size)
        offset-y (/ game-height 2 scaled-tile-size)
        min-y (- y offset-y 1)
        max-y (+ y offset-y)]
    {:camera (t/translate orig-camera (- x offset-x) (- y offset-y))
     :window-anchor window
     :player-anchor player
     :min-y min-y
     :max-y max-y}))

(defn update-mouse [window mouse player player-size]
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
               (- (:width player-size)))
        wy (-> wy
               (+ (:y player))
               (- (:height player-size)))]
   {:world-coords {:x wx :y wy}}))

(declare restart!)

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
            :when (= (:kind entity) :player)
            direction Direction
            :when (= (:id direction) (:id entity))]
        entity))
    :get-enemies
    (fn []
      (let [entity Entity
            :accumulator (acc/all)
            :when (not= (:kind entity) :player)]
        entity))
    :get-current-image
    (fn [?id]
      (let [current-image CurrentImage
            :when (= ?id (:id current-image))]
        (:value current-image)))
    :get-size
    (fn [?id]
      (let [size Size
            :when (= ?id (:id size))]
        size))
    :get-tiled-map
    (fn []
      (let [tiled-map TiledMap]
        tiled-map))
    :get-enemy-under-cursor
    (fn []
      (let [distance DistanceFromCursor
            :accumulator (acc/min :value :returns-fact true)
            :when (<= (:value distance) max-cursor-distance)
            target Entity
            :when (and (= (:id target) (:id distance))
                       (not= (:kind target) :player))]
        target))})

(def rules
  '{:move-enemy
    (let [game Game
          player Entity
          :when (= (:kind player) :player)
          player-health Health
          :when (= (:id player) (:id player-health))
          enemy Entity
          :when (and (not= (:game-anchor enemy) game)
                     (not= (:kind enemy) :player))
          enemy-health Health
          :when (and (= (:id enemy) (:id enemy-health))
                     (> (:value enemy-health) 0))]
      (clarax/merge! enemy (-> (move/get-enemy-velocity enemy player (:value player-health))
                               (move/move enemy game)
                               (assoc :game-anchor game))))
    :move-player
    (let [game Game
          window Window
          keys Keys
          mouse Mouse
          player Entity
          :when (and (not= (:game-anchor player) game)
                     (= (:kind player) :player))
          player-health Health
          :when (and (= (:id player) (:id player-health))
                     (> (:value player-health) 0))]
      (clarax/merge! player (-> (move/get-player-velocity window (:pressed keys) mouse player)
                                (move/move player game)
                                (assoc :game-anchor game))))
    :update-distance-from-cursor
    (let [mouse Mouse
          :when (not= nil (:world-coords mouse))
          entity Entity
          distance DistanceFromCursor
          :when (= (:id entity) (:id distance))]
      (clarax/merge! distance {:value (move/calc-distance entity (:world-coords mouse))}))
    :update-distance-from-player
    (let [player Entity
          :when (= (:kind player) :player)
          enemy Entity
          :when (not= (:id player) (:id enemy))
          distance DistanceFromPlayer
          :when (= (:id distance) (:id enemy))]
      (clarax/merge! distance {:value (move/calc-distance enemy player)}))
    :update-camera
    (let [window Window
          :when (or (pos? (:width window))
                    (pos? (:height window)))
          player Entity
          :when (= (:kind player) :player)
          camera Camera
          :when (or (not= (:window-anchor camera) window)
                    (not= (:player-anchor camera) player))]
      (clarax/merge! camera (update-camera window player)))
    :animate
    (let [game Game
          entity Entity
          direction Direction
          :when (= (:id entity) (:id direction))
          health Health
          :when (= (:id entity) (:id health))
          current-image CurrentImage
          :when (= (:id entity) (:id current-image))
          animation Animation
          :accumulator (acc/all)
          :when (= (:id entity) (:entity-id animation))]
      (let [ret (move/animate entity (:value health) (:value direction) game animation)]
        (some->> (:current-image ret)
                 (hash-map :value)
                 (clarax/merge! current-image))
        (some->> (:direction ret)
                 (hash-map :value)
                 (clarax/merge! direction))))
    :dont-overlap-tile
    (let [tiled-map TiledMap
          entity Entity
          :when (or (not= 0 (:x-change entity))
                    (not= 0 (:y-change entity)))
          size Size
          :when (= (:id entity) (:id size))]
      (some->> (move/dont-overlap-tile entity size tiled-map)
               (clarax/merge! entity)))
    :player-attack-with-key
    (let [game Game
          player Entity
          :when (= (:kind player) :player)
          attack-delay AttackDelay
          :when (= (:id player) (:id attack-delay))
          last-attack LastAttack
          :when (and (= (:id player) (:id last-attack))
                     (-> (:total-time game)
                         (- (:value last-attack))
                         (>= (:value attack-delay))))
          direction Direction
          :when (= (:id direction) (:id player))
          keys Keys
          :when (contains? (:pressed keys) :space)
          distance DistanceFromPlayer
          :accumulator (acc/min :value :returns-fact true)
          :when (and (<= (:value distance) move/max-attack-distance)
                     (not= (:id distance) (:id player)))
          target Entity
          :when (= (:id target) (:id distance))]
      (clarax/merge! last-attack {:value (:total-time game)})
      (clara/insert-unconditional! (->Attack (:id player) (:id target)))
      (some->> (move/get-direction
                 (- (:x target) (:x player))
                 (- (:y target) (:y player)))
               (hash-map :value)
               (clarax/merge! direction)))
    :player-attack-with-mouse
    (let [game Game
          player Entity
          :when (= (:kind player) :player)
          attack-delay AttackDelay
          :when (= (:id player) (:id attack-delay))
          last-attack LastAttack
          :when (and (= (:id player) (:id last-attack))
                     (-> (:total-time game)
                         (- (:value last-attack))
                         (>= (:value attack-delay))))
          direction Direction
          :when (= (:id direction) (:id player))
          mouse Mouse
          :when (and (= (:button mouse) :right)
                     (not= nil (:world-coords mouse)))
          distance DistanceFromCursor
          :accumulator (acc/min :value :returns-fact true)
          :when (and (<= (:value distance) max-cursor-distance)
                     (not= (:id distance) (:id player)))
          target Entity
          :when (= (:id target) (:id distance))]
      (clarax/merge! last-attack {:value (:total-time game)})
      (clara/insert-unconditional! (->Attack (:id player) (:id target)))
      (let [{:keys [x y]} (:world-coords mouse)]
        (some->> (move/get-direction
                   (- x (:x player))
                   (- y (:y player)))
                 (hash-map :value)
                 (clarax/merge! direction))))
    :enemy-attack
    (let [game Game
          distance DistanceFromPlayer
          :when (<= (:value distance) move/max-attack-distance)
          enemy Entity
          :when (and (not= (:kind enemy) :player)
                     (= (:id enemy) (:id distance)))
          attack-delay AttackDelay
          :when (= (:id enemy) (:id attack-delay))
          last-attack LastAttack
          :when (and (= (:id enemy) (:id last-attack))
                     (-> (:total-time game)
                         (- (:value last-attack))
                         (>= (:value attack-delay))))
          player Entity
          :when (= (:kind player) :player)]
      (clarax/merge! last-attack {:value (:total-time game)})
      (clara/insert-unconditional! (->Attack (:id enemy) (:id player))))
    :update-mouse-world-coords
    (let [window Window
          mouse Mouse
          :when (= nil (:world-coords mouse))
          player Entity
          :when (= (:kind player) :player)
          player-size Size
          :when (= (:id player) (:id player-size))]
      (clarax/merge! mouse (update-mouse window mouse player player-size)))
    :attack
    (let [game Game
          attack Attack
          source Entity
          :when (= (:id source) (:source-id attack))
          source-damage Damage
          :when (= (:id source) (:id source-damage))
          target Entity
          :when (= (:id target) (:target-id attack))
          target-health Health
          :when (= (:id target) (:id target-health))]
      (clara/retract! attack)
      (when (<= (move/calc-distance source target)
                move/max-attack-distance)
        (let [duration (+ (:total-time game) animation-duration)
              sound-file (if (= (:kind source) :player)
                           "monsterhurt.wav"
                           "playerhurt.wav")]
          (utils/play-sound! sound-file)
          (->> duration
               (->Animation (:id source) :attacks)
               clara/insert-unconditional!)
          (->> duration
               (->Animation (:id target) :hits)
               clara/insert-unconditional!)
          (clarax/merge! target-health {:value (- (:value target-health)
                                                  (:value source-damage))}))))
    :death
    (let [entity Entity
          health Health
          :when (and (= (:id entity) (:id health))
                     (<= (:value health) 0))
          distance-from-player DistanceFromPlayer
          :when (= (:id entity) (:id distance-from-player))
          distance-from-cursor DistanceFromCursor
          :when (= (:id entity) (:id distance-from-cursor))]
      (clara/retract! distance-from-player)
      (clara/retract! distance-from-cursor)
      (utils/play-sound! "death.wav")
      (when (= (:kind entity) :player)
        (restart!)))
    :remove-expired-animations
    (let [game Game
          animation Animation
          :when (<= (:expire-time animation) (:total-time game))]
      (clara/retract! animation))})

#?(:clj (defmacro merge-into-session []
          (list '->session (merge queries rules))))

(def initial-session (merge-into-session))

(defonce *session (atom nil))
(defonce *reload? (atom false))

;; when this ns is reloaded, reload the session
(when @*session
  (reset! *reload? true))

(def restart-delay 1000)

(defn restart! []
  #?(:clj (future
            (Thread/sleep restart-delay)
            (reset! *reload? true))
     :cljs (js/setTimeout #(reset! *reload? true) restart-delay)))

