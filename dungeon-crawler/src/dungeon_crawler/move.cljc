(ns dungeon-crawler.move
  (:require [dungeon-crawler.tiles :as tiles]
            [dungeon-crawler.entities :as e]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(def damping 0.1)
(def max-velocity 4)
(def max-enemy-velocity (/ max-velocity 2))
(def max-movement-per-frame 0.5)
(def min-movement-per-frame -0.5)
(def deceleration 0.8)
(def animation-secs 0.2)
(def max-attack-distance 0.5)
(def max-cursor-distance 0.5)
(def min-aggro-distance (- max-attack-distance 0.1))
(def max-aggro-distance 2)

(defn decelerate
  [velocity]
  (let [velocity (* velocity deceleration)]
    (if (< (math abs velocity) damping)
      0
      velocity)))

(defn get-player-velocity
  [window-width window-height
   pressed-keys
   mouse-x mouse-y mouse-button
   {:keys [x y x-velocity y-velocity]}]
  (if (= :left mouse-button)
    (let [x (float (- mouse-x (/ window-width 2)))
          y (float (- mouse-y (/ window-height 2)))
          x-adjust (if (== y 0)
                     0
                     (* max-velocity (math abs (/ x y))))
          y-adjust (if (== x 0)
                     0
                     (* max-velocity (math abs (/ y x))))]
      [(* (math #?(:clj signum :cljs sign) x)
          (min max-velocity x-adjust))
       (* (math #?(:clj signum :cljs sign) y)
          (min max-velocity y-adjust))])
    [(cond
       (contains? pressed-keys :left)
       (* -1 max-velocity)
       (contains? pressed-keys :right)
       max-velocity
       :else
       x-velocity)
     (cond
       (contains? pressed-keys :up)
       (* -1 max-velocity)
       (contains? pressed-keys :down)
       max-velocity
       :else
       y-velocity)]))

(defn calc-distance [x1 y1 x2 y2]
  (math abs (math sqrt (+ (math pow (- x1 x2) 2)
                          (math pow (- y1 y2) 2)))))

(defn get-enemy-velocity [{:keys [x-velocity y-velocity] :as enemy} player distance-from-player]
  (or (when (> (:health player) 0)
        (cond
          (< min-aggro-distance distance-from-player max-aggro-distance)
          [(cond-> max-enemy-velocity
                   (< (:x player) (:x enemy))
                   (* -1))
           (cond-> max-enemy-velocity
                   (< (:y player) (:y enemy))
                   (* -1))]
          (<= distance-from-player min-aggro-distance)
          [0 0]))
      [(if (= 0 x-velocity)
         (-> (rand-int 3)
             (- 1)
             (* max-enemy-velocity))
         x-velocity)
       (if (= 0 y-velocity)
         (-> (rand-int 3)
             (- 1)
             (* max-enemy-velocity))
         y-velocity)]))

(defn get-direction
  [x-velocity y-velocity]
  (some->> e/velocities
           (filter (fn [[x y]]
                     (and (= x (int (math #?(:clj signum :cljs sign) (float x-velocity))))
                          (= y (int (math #?(:clj signum :cljs sign) (float y-velocity)))))))
           first
           (.indexOf e/velocities)
           (nth e/directions)))

(defn dont-overlap-tile
  [x y
   x-change y-change
   width height
   tiled-map-layers]
  (let [old-x (- x x-change)
        old-y (- y y-change)
        touching-x? (tiles/touching-tile? tiled-map-layers "walls" x old-y width height)
        touching-y? (tiles/touching-tile? tiled-map-layers "walls" old-x y width height)]
    (when (or touching-x? touching-y?)
      (cond-> {::e/x-change 0 ::e/y-change 0}
              touching-x?
              (assoc ::e/x-velocity 0 ::e/x old-x)
              touching-y?
              (assoc ::e/y-velocity 0 ::e/y old-y)))))

(defn move [x y x-velocity y-velocity delta-time]
  (let [x-change (-> (* x-velocity delta-time)
                     (max min-movement-per-frame)
                     (min max-movement-per-frame))
        y-change (-> (* y-velocity delta-time)
                     (max min-movement-per-frame)
                     (min max-movement-per-frame))]
    (when (or (not= 0 x-change)
              (not= 0 y-change))
      {::e/x-velocity (decelerate x-velocity)
       ::e/y-velocity (decelerate y-velocity)
       ::e/x-change x-change
       ::e/y-change y-change
       ::e/x (+ x x-change)
       ::e/y (+ y y-change)})))

(defn animate
  [{:keys [x-velocity y-velocity moves animation health direction] :as character}
   total-time]
  (or (when (<= health 0)
        {::e/current-image (get-in character [:deads direction])})
      (when-let [images (get character animation)]
        {::e/current-image (get images direction)})
      (when-let [direction (get-direction x-velocity y-velocity)]
        {::e/current-image
         (when (or (not= x-velocity 0)
                   (not= y-velocity 0))
           (let [images (direction moves)
                 cycle-time (mod total-time (* animation-secs (count images)))]
             (nth images (int (/ cycle-time animation-secs)))))
         ::e/direction direction})
      {::e/current-image (get-in moves [direction 0])}))

