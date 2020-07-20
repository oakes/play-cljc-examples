(ns dungeon-crawler.session
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.move :as move]
            [dungeon-crawler.entities :as e]
            [dungeon-crawler.tiles :as tiles]
            [play-cljc.gl.entities-2d :as e2d]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [odoyle.rules :as o]))

(def orig-camera (e2d/->camera))
(def vertical-tiles 7)
(def animation-duration 0.5)

(defn update-camera [game-width game-height x y]
  (let [scaled-tile-size (/ game-height vertical-tiles)
        offset-x (/ game-width 2 scaled-tile-size)
        offset-y (/ game-height 2 scaled-tile-size)
        min-y (- y offset-y 1)
        max-y (+ y offset-y)]
    {:camera (t/translate orig-camera (- x offset-x) (- y offset-y))
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

(def queries
  (o/ruleset
    {::get-window
     [:what
      [::window ::width width]
      [::window ::height height]]
     ::get-camera
     [:what
      [::camera ::camera camera]
      [::camera ::min-y min-y]
      [::camera ::max-y max-y]]
     ::get-mouse
     [:what
      [::mouse ::x x]
      [::mouse ::y y]
      [::mouse ::world-coords world-coords]
      [::mouse ::button button]]
     ::get-keys
     [:what
      [::keys ::pressed pressed]]
     ::get-entity
     [:what
      [id ::e/kind kind]
      [id ::e/moves moves]
      [id ::e/attacks attacks]
      [id ::e/specials specials]
      [id ::e/hits hits]
      [id ::e/deads deads]
      [id ::e/x x]
      [id ::e/y y]
      [id ::e/x-change x-change]
      [id ::e/y-change y-change]
      [id ::e/x-velocity x-velocity]
      [id ::e/y-velocity y-velocity]
      [id ::e/direction direction]
      [id ::e/current-image current-image]
      [id ::e/width width]
      [id ::e/height height]]
     ::get-tiled-map
     [:what
      [::tiles/tiled-map ::tiles/layers layers]
      [::tiles/tiled-map ::tiles/width width]
      [::tiles/tiled-map ::tiles/height height]
      [::tiles/tiled-map ::tiles/entities entities]]
     ::get-distance-from-cursor
     [:what
      [id ::distance-from-cursor distance]
      :when
      (<= distance move/max-cursor-distance)]}))

(defn get-enemy-under-cursor [session]
  (when-let [distances (not-empty (o/query-all session ::get-distance-from-cursor))]
    (->> distances
         (apply min-key :distance)
         :distance)))

(def rules
  (o/ruleset
    {::move-enemy
     [:what
      [::time ::delta delta-time]
      [pid ::e/kind :player]
      [pid ::e/health player-health]
      [pid ::e/x player-x]
      [pid ::e/y player-y]
      [pid ::e/x-velocity player-x-velocity]
      [pid ::e/y-velocity player-y-velocity]
      [eid ::e/kind enemy-kind]
      [eid ::e/health enemy-health]
      [eid ::e/x enemy-x {:then false}]
      [eid ::e/y enemy-y {:then false}]
      [eid ::e/x-velocity enemy-x-velocity {:then false}]
      [eid ::e/y-velocity enemy-y-velocity {:then false}]
      [eid ::distance-from-player distance-from-player]
      :when
      (not= enemy-kind :player)
      (> enemy-health 0)
      :then
      (let [enemy {:x enemy-x :y enemy-y :x-velocity enemy-x-velocity :y-velocity enemy-y-velocity}
            player {:x player-x :y player-y :x-velocity player-x-velocity :y-velocity player-y-velocity}
            [xv yv] (move/get-enemy-velocity enemy player player-health distance-from-player)]
        (some->> (move/move enemy-x enemy-y xv yv delta-time)
                 (o/insert! eid)))]
     ::move-player
     [:what
      [::time ::delta delta-time]
      [::window ::width width]
      [::window ::height height]
      [::keys ::pressed pressed]
      [::mouse ::x mouse-x]
      [::mouse ::y mouse-y]
      [::mouse ::button mouse-button]
      [pid ::e/kind :player]
      [pid ::e/health player-health]
      [pid ::e/x player-x {:then false}]
      [pid ::e/y player-y {:then false}]
      [pid ::e/x-velocity player-x-velocity {:then false}]
      [pid ::e/y-velocity player-y-velocity {:then false}]
      :when
      (> player-health 0)
      :then
      (let [player {:x player-x :y player-y :x-velocity player-x-velocity :y-velocity player-y-velocity}
            [xv yv] (move/get-player-velocity width height pressed mouse-x mouse-y mouse-button player)]
        (some->> (move/move player-x player-y xv yv delta-time)
                 (o/insert! pid)))]
     ::update-camera
     [:what
      [::window ::width width]
      [::window ::height height]
      [id ::e/kind :player]
      [id ::e/x x]
      [id ::e/y y]
      [::camera ::camera camera {:then false}]
      :then
      (let [{:keys [camera min-y max-y]} (update-camera width height x y)]
        (o/insert! ::camera
                   {::camera camera
                    ::min-y min-y
                    ::max-y max-y}))]}))

#_
(def rules
  '{:update-distance-from-cursor
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
          player-damage Damage
          :when (= (:id player) (:id player-damage))
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
          :when (= (:id target) (:id distance))
          target-health Health
          :when (= (:id target) (:id target-health))]
      (clarax/merge! last-attack {:value (:total-time game)})
      (attack! game player player-damage target target-health)
      (some->> (move/get-direction
                 (- (:x target) (:x player))
                 (- (:y target) (:y player)))
               (hash-map :value)
               (clarax/merge! direction)))
    :player-attack-with-mouse
    (let [game Game
          player Entity
          :when (= (:kind player) :player)
          player-damage Damage
          :when (= (:id player) (:id player-damage))
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
          distance-from-cursor DistanceFromCursor
          :accumulator (acc/min :value :returns-fact true)
          :when (and (<= (:value distance-from-cursor) move/max-cursor-distance)
                     (not= (:id distance-from-cursor) (:id player)))
          target Entity
          :when (= (:id target) (:id distance-from-cursor))
          target-health Health
          :when (= (:id target) (:id target-health))
          distance-from-player DistanceFromPlayer
          :when (and (<= (:value distance-from-player) move/max-attack-distance)
                     (= (:id distance-from-player) (:id target)))]
      (clarax/merge! last-attack {:value (:total-time game)})
      (attack! game player player-damage target target-health)
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
          enemy-damage Damage
          :when (= (:id enemy) (:id enemy-damage))
          attack-delay AttackDelay
          :when (= (:id enemy) (:id attack-delay))
          last-attack LastAttack
          :when (and (= (:id enemy) (:id last-attack))
                     (-> (:total-time game)
                         (- (:value last-attack))
                         (>= (:value attack-delay))))
          player Entity
          :when (= (:kind player) :player)
          player-health Health
          :when (and (= (:id player) (:id player-health))
                     (> (:value player-health) 0))]
      (clarax/merge! last-attack {:value (:total-time game)})
      (attack! game enemy enemy-damage player player-health))
    :update-mouse-world-coords
    (let [window Window
          mouse Mouse
          :when (= nil (:world-coords mouse))
          player Entity
          :when (= (:kind player) :player)
          player-size Size
          :when (= (:id player) (:id player-size))]
      (clarax/merge! mouse (update-mouse window mouse player player-size)))
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

(def initial-session
  (reduce o/add-rule (o/->session)
          (concat queries rules)))

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

#_
(defn attack! [game source source-damage target target-health]
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
                                            (:value source-damage))})))

