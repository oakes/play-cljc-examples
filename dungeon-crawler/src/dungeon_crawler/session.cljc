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
           mouse-x mouse-y
           player-x player-y
           player-width player-height]}]
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
               (+ player-x)
               (- player-width))
        wy (-> wy
               (+ player-y)
               (- player-height))]
   [wx wy]))

(defn get-enemy-under-cursor [session]
  (when-let [enemies (not-empty (o/query-all session ::get-enemy-distance-from-cursor))]
    (apply min-key :distance enemies)))

(defn get-enemy-near-player [session]
  (when-let [enemies (not-empty (o/query-all session ::get-enemy-distance-from-player))]
    (apply min-key :distance enemies)))

(defn get-enemy-near-player-that-can-attack [session total-time]
  (when-let [enemies (not-empty (o/query-all session ::get-enemy-distance-from-player))]
    (when-let [enemies (not-empty (filter (fn [{:keys [last-attack attack-delay]}]
                                            (-> total-time
                                                (- last-attack)
                                                (>= attack-delay)))
                                          enemies))]
      (apply min-key :distance enemies))))

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
      [::mouse ::world-x world-x]
      [::mouse ::world-y world-y]
      [::mouse ::button button]]

     ::get-keys
     [:what
      [::keys ::pressed pressed]]

     ::get-entity
     [:what
      [id ::e/kind kind]
      [id ::e/x x]
      [id ::e/y y]
      [id ::e/x-change x-change]
      [id ::e/y-change y-change]
      [id ::e/x-velocity x-velocity]
      [id ::e/y-velocity y-velocity]
      [id ::e/current-image current-image]
      [id ::e/width width]
      [id ::e/height height]]

     ::get-tiled-map
     [:what
      [::tiles/tiled-map ::tiles/layers layers]
      [::tiles/tiled-map ::tiles/width width]
      [::tiles/tiled-map ::tiles/height height]
      [::tiles/tiled-map ::tiles/entities entities]]

     ::get-enemy-distance-from-cursor
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

     ::get-enemy-distance-from-player
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
      (<= distance move/max-attack-distance)]}))

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
      [eid ::distance-from-player distance-from-player {:then false}]
      :when
      (not= enemy-kind :player)
      (> enemy-health 0)
      :then
      (let [enemy {:x enemy-x :y enemy-y :x-velocity enemy-x-velocity :y-velocity enemy-y-velocity}
            player {:x player-x :y player-y :x-velocity player-x-velocity :y-velocity player-y-velocity}
            [xv yv] (move/get-enemy-velocity enemy player player-health distance-from-player)]
        (some->> (move/move enemy-x enemy-y xv yv delta-time)
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
                 (o/insert o/*session* pid)
                 o/reset!))]

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
      [pid ::e/kind :player]
      [pid ::e/width player-width]
      [pid ::e/height player-height]
      [pid ::e/x player-x]
      [pid ::e/y player-y]
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
      [pid ::e/kind :player]
      [pid ::e/x player-x]
      [pid ::e/y player-y]
      [eid ::e/x enemy-x]
      [eid ::e/y enemy-y]
      :when
      (not= eid pid)
      :then
      (-> o/*session*
          (o/insert eid ::distance-from-player (move/calc-distance player-x player-y enemy-x enemy-y))
          o/reset!)]

     ::animate
     [:what
      [::time ::total total-time]
      [id ::e/direction direction {:then false}]
      [id ::e/health health]
      [id ::e/current-image current-image {:then false}]
      [id ::e/x-velocity xv]
      [id ::e/y-velocity yv]
      [id ::e/moves moves]
      [id ::e/attacks attacks]
      [id ::e/hits hits]
      [id ::e/deads deads]
      [id ::e/current-animation animation]
      :then
      (->> (move/animate {:x-velocity xv :y-velocity yv :moves moves :attacks attacks :hits hits :deads deads}
                         animation health direction total-time)
           (o/insert o/*session* id)
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
      [pid ::e/kind :player]
      [pid ::e/damage damage]
      [pid ::e/attack-delay attack-delay]
      [pid ::e/last-attack last-attack]
      [pid ::e/x x]
      [pid ::e/y y]
      :when
      (or (contains? pressed :space)
          (= button :right))
      (-> total-time
          (- last-attack)
          (>= attack-delay))
      :then
      (when-let [enemy (get-enemy-near-player o/*session*)]
        (o/reset! (attack o/*session* total-time {:id pid :damage damage :kind :player :x x :y y} enemy)))]

     ::enemy-attack
     [:what
      [::time ::total total-time]
      [pid ::e/kind :player]
      [pid ::e/health health]
      [pid ::e/x x]
      [pid ::e/y y]
      :then
      (when-let [enemy (get-enemy-near-player-that-can-attack o/*session* total-time)]
        (o/reset! (attack o/*session* total-time enemy {:id pid :health health :x x :y y})))]

     ::death
     [:what
      [id ::e/kind kind]
      [id ::e/health health]
      :when
      (<= health 0)
      :then
      (-> o/*session*
          (o/retract id ::e/health)
          (o/retract id ::distance-from-cursor)
          o/reset!)
      (utils/play-sound! "death.wav")
      (when (= kind :player)
        (restart!))]}))

(def initial-session
  (reduce o/add-rule (o/->session)
          (concat queries rules)))

;; when this ns is reloaded, reload the session
(when @*session
  (reset! *reload? true))

