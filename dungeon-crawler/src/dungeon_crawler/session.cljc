(ns dungeon-crawler.session
  (:require [dungeon-crawler.utils :as utils]
            [dungeon-crawler.move :as move]
            [dungeon-crawler.entities :as e]
            [dungeon-crawler.tiles :as tiles]
            [play-cljc.gl.entities-2d :as e2d]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [odoyle.rules :as o #?(:clj :refer :cljs :refer-macros) [ruleset]]))

(def orig-camera (e2d/->camera))
(def vertical-tiles 7)
(def animation-duration 0.5)

(defonce *session (atom nil))
(defonce *reload? (atom false))

(def restart-delay 1000)

(defn restart! []
  #?(:clj (future
            (Thread/sleep restart-delay)
            (reset! *reload? true))
     :cljs (js/setTimeout #(reset! *reload? true) restart-delay)))

(defn attack [session total-time source target]
  (let [duration (+ total-time animation-duration)
        sound-file (if (= (:kind source) :player)
                     "monsterhurt.wav"
                     "playerhurt.wav")
        new-direction (move/get-direction
                        (- (:x target) (:x source))
                        (- (:y target) (:y source)))]
    (utils/play-sound! sound-file)
    (-> session
        (o/insert (:id source)
                  {::e/current-animation :attacks
                   ::e/animation-expiration duration
                   ::e/last-attack total-time})
        (o/insert (:id target)
                  {::e/current-animation :hits
                   ::e/animation-expiration duration
                   ::e/health (- (:health target) (:damage source))})
        (cond-> new-direction
                (o/insert (:id source) ::e/direction new-direction)))))

(defn update-camera [game-width game-height x y]
  (let [scaled-tile-size (/ game-height vertical-tiles)
        offset-x (/ game-width 2 scaled-tile-size)
        offset-y (/ game-height 2 scaled-tile-size)
        min-y (- y offset-y 1)
        max-y (+ y offset-y)]
    {:camera (t/translate orig-camera (- x offset-x) (- y offset-y))
     :min-y min-y
     :max-y max-y}))

(defn get-mouse-world-coords
  [{:keys [window-width window-height
           mouse-x mouse-y player]}]
  (let [;; convert mouse coords to (-1 to 1) coords
        matrix (->> (m/projection-matrix window-width window-height)
                    (m/multiply-matrices 3 (m/translation-matrix mouse-x mouse-y)))
        wx (nth matrix 6)
        wy (* -1 (nth matrix 7))
        ;; convert to tile coords
        y-multiplier (/ vertical-tiles 2)
        x-multiplier (* y-multiplier (/ window-width window-height))
        wx (* wx x-multiplier)
        wy (* wy y-multiplier)
        ;; make mouse relative to player position
        wx (-> wx
               (+ (:x player))
               (- (:width player)))
        wy (-> wy
               (+ (:y player))
               (- (:height player)))]
   [wx wy]))

(defn get-enemy-under-cursor [session]
  (when-let [enemies (not-empty (o/query-all session ::enemy-near-cursor))]
    (apply min-key :distance enemies)))

(defn get-enemy-near-player [session]
  (when-let [enemies (not-empty (o/query-all session ::enemy-near-player))]
    (apply min-key :distance enemies)))

(def rules
  (o/ruleset
    {::window
     [:what
      [::window ::width width]
      [::window ::height height]]

     ::camera
     [:what
      [::camera ::camera camera]
      [::camera ::min-y min-y]
      [::camera ::max-y max-y]]

     ::mouse
     [:what
      [::mouse ::x x]
      [::mouse ::y y]
      [::mouse ::world-x world-x]
      [::mouse ::world-y world-y]
      [::mouse ::button button]]

     ::keys
     [:what
      [::keys ::pressed pressed]]

     ::entity
     [:what
      [id ::e/kind kind]
      [id ::e/x x]
      [id ::e/y y]
      [id ::e/x-velocity x-velocity]
      [id ::e/y-velocity y-velocity]
      [id ::e/current-image current-image]
      [id ::e/width width]
      [id ::e/height height]
      [id ::e/health health]
      [id ::e/damage damage]
      [id ::e/attack-delay attack-delay]
      [id ::e/last-attack last-attack]
      [id ::e/direction direction]
      [id ::e/moves moves]
      [id ::e/attacks attacks]
      [id ::e/hits hits]
      [id ::e/deads deads]
      [id ::e/current-animation animation]
      :then
      ;; create a derived "all" fact that contains
      ;; all the fields above in a single map
      (-> o/*session*
          (o/insert ;; give the player's "all" fact a unique id
                    ;; so it can be easily distinguished when
                    ;; pulled into other rules
                    (if (= kind :player) ::player id)
                    ::all
                    o/*match*)
          o/reset!)]

     ::tiled-map
     [:what
      [::tiles/tiled-map ::tiles/layers layers]
      [::tiles/tiled-map ::tiles/width width]
      [::tiles/tiled-map ::tiles/height height]
      [::tiles/tiled-map ::tiles/entities entities]]

     ::enemy-near-cursor
     [:what
      [id ::e/kind kind]
      [id ::e/health health]
      [id ::e/x x]
      [id ::e/y y]
      [id ::distance-from-cursor distance]
      :when
      (not= kind :player)
      (> health 0)
      (<= distance move/max-cursor-distance)]

     ::enemy-near-player
     [:what
      [id ::e/kind kind]
      [id ::e/health health]
      [id ::e/x x]
      [id ::e/y y]
      [id ::distance-from-player distance]
      [id ::e/last-attack last-attack]
      [id ::e/attack-delay attack-delay]
      [id ::e/damage damage]
      :when
      (not= kind :player)
      (> health 0)
      (<= distance move/max-attack-distance)
      ;; create a derived fact that contains all nearby enemies.
      ;; we use :then-finally because it runs
      ;; after matches of this rule are inserted *and* retracted.
      ;; :then blocks are only run after insertions.
      :then-finally
      (->> (o/query-all o/*session* ::enemy-near-player)
           (o/insert o/*session* ::derived ::nearby-enemies)
           o/reset!)]

     ::move-enemy
     [:what
      [::time ::delta delta-time]
      [::player ::all player {:then false}]
      [eid ::all enemy {:then false}]
      [eid ::distance-from-player distance-from-player {:then false}]
      :when
      (not= eid ::player)
      (> (:health enemy) 0)
      :then
      (let [[xv yv] (move/get-enemy-velocity enemy player distance-from-player)]
        (some->> (move/move (:x enemy) (:y enemy) xv yv delta-time)
                 (o/insert o/*session* eid)
                 o/reset!))]

     ::move-player
     [:what
      [::time ::delta delta-time]
      [::window ::width width]
      [::window ::height height]
      [::keys ::pressed pressed]
      [::mouse ::x mouse-x]
      [::mouse ::y mouse-y]
      [::mouse ::button mouse-button]
      [::player ::all player {:then false}]
      :when
      (> (:health player) 0)
      :then
      (let [[xv yv] (move/get-player-velocity width height pressed mouse-x mouse-y mouse-button player)]
        (some->> (move/move (:x player) (:y player) xv yv delta-time)
                 (o/insert o/*session* (:id player))
                 o/reset!))]

     ::update-camera
     [:what
      [::window ::width width]
      [::window ::height height]
      [::player ::all player]
      :then
      (let [{:keys [camera min-y max-y]} (update-camera width height (:x player) (:y player))]
        (-> o/*session*
            (o/insert ::camera
                      {::camera camera
                       ::min-y min-y
                       ::max-y max-y})
            o/reset!))]

     ::update-world-coords
     [:what
      [::window ::width window-width]
      [::window ::height window-height]
      [::mouse ::x mouse-x]
      [::mouse ::y mouse-y]
      [::player ::all player]
      :then
      (let [[wx wy] (get-mouse-world-coords o/*match*)]
        (-> o/*session*
            (o/insert ::mouse
                      {::world-x wx
                       ::world-y wy})
            o/reset!))]

     ::update-distance-from-cursor
     [:what
      [::mouse ::world-x world-x]
      [::mouse ::world-y world-y]
      [id ::e/x x]
      [id ::e/y y]
      :then
      (-> o/*session*
          (o/insert id ::distance-from-cursor (move/calc-distance x y world-x world-y))
          o/reset!)]

     ::update-distance-from-player
     [:what
      [::player ::all player]
      [eid ::all enemy]
      :when
      (not= eid ::player)
      :then
      (-> o/*session*
          (o/insert eid ::distance-from-player (move/calc-distance (:x player) (:y player) (:x enemy) (:y enemy)))
          o/reset!)]

     ::animate
     [:what
      [::time ::total total-time]
      [id ::all entity {:then false}]
      :then
      (->> (move/animate entity total-time)
           (o/insert o/*session* (:id entity))
           o/reset!)]

     ::remove-expired-animations
     [:what
      [::time ::total total-time]
      [id ::e/current-animation animation {:then false}]
      [id ::e/animation-expiration expiration]
      :when
      (not= :none animation)
      (> total-time expiration)
      :then
      (-> o/*session*
          (o/insert id ::e/current-animation :none)
          o/reset!)]

     ::dont-overlap-tile
     [:what
      [id ::e/x x]
      [id ::e/y y]
      [id ::e/x-change x-change]
      [id ::e/y-change y-change]
      [id ::e/width width]
      [id ::e/height height]
      [::tiles/tiled-map ::tiles/layers layers]
      :when
      (or (not (== 0 x-change))
          (not (== 0 y-change)))
      :then
      (some->> (move/dont-overlap-tile x y x-change y-change width height layers)
               (o/insert o/*session* id)
               o/reset!)]

     ::player-attack
     [:what
      [::time ::total total-time]
      [::keys ::pressed pressed]
      [::mouse ::button button]
      [::player ::all player]
      :when
      (or (contains? pressed :space)
          (= button :right))
      (-> total-time
          (- (:last-attack player))
          (>= (:attack-delay player)))
      :then
      (when-let [enemy (get-enemy-near-player o/*session*)]
        (o/reset! (attack o/*session* total-time player enemy)))]

     ::enemy-attack
     [:what
      [::time ::total total-time]
      [::player ::all player]
      [::derived ::nearby-enemies enemies]
      :then
      (when-let [enemy (some (fn [{:keys [last-attack attack-delay] :as enemy}]
                               (when (-> total-time
                                         (- last-attack)
                                         (>= attack-delay))
                                 enemy))
                             enemies)]
        (o/reset! (attack o/*session* total-time enemy player)))]

     ::death
     [:what
      [id ::e/kind kind]
      [id ::e/health health]
      :when
      (<= health 0)
      :then
      (-> o/*session*
          (o/retract id ::distance-from-cursor)
          o/reset!)
      (utils/play-sound! "death.wav")
      (when (= kind :player)
        (restart!))]}))

(def initial-session
  (reduce o/add-rule (o/->session) rules))

;; when this ns is reloaded, reload the session
(when @*session
  (reset! *reload? true))

