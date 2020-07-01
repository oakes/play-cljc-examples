(ns dungeon-crawler.move
  (:require [dungeon-crawler.tiles :as tiles]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(def damping 0.1)
(def max-velocity 4)
(def max-enemy-velocity (/ max-velocity 2))
(def max-movement-per-frame 0.5)
(def min-movement-per-frame -0.5)
(def deceleration 0.8)
(def animation-secs 0.2)
(def directions [:w :nw :n :ne
                 :e :se :s :sw])
(def velocities [[-1 0] [-1 -1] [0 -1] [1 -1]
                 [1 0] [1 1] [0 1] [-1 1]])
(def max-attack-distance 0.5)
(def min-aggro-distance (- max-attack-distance 0.1))
(def max-aggro-distance 2)

(defn decelerate
  [velocity]
  (let [velocity (* velocity deceleration)]
    (if (< (math abs velocity) damping)
      0
      velocity)))

(defn get-player-velocity
  [{:keys [width height] :as window}
   pressed-keys
   mouse
   {:keys [x y x-velocity y-velocity]}]
  (if (= :left (:button mouse))
    (let [x (float (- (:x mouse) (/ width 2)))
          y (float (- (:y mouse) (/ height 2)))
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

(defn calc-distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  (math abs (math sqrt (+ (math pow (- x1 x2) 2)
                          (math pow (- y1 y2) 2)))))

(defn get-enemy-velocity [{:keys [x-velocity y-velocity] :as enemy} player player-health]
  (or (when (> player-health 0)
        (let [distance (calc-distance enemy player)]
          (cond
            (< min-aggro-distance distance max-aggro-distance)
            [(cond-> max-enemy-velocity
                     (< (:x player) (:x enemy))
                     (* -1))
             (cond-> max-enemy-velocity
                     (< (:y player) (:y enemy))
                     (* -1))]
            (<= distance min-aggro-distance)
            [0 0])))
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
  (some->> velocities
           (filter (fn [[x y]]
                     (and (= x (int (math #?(:clj signum :cljs sign) (float x-velocity))))
                          (= y (int (math #?(:clj signum :cljs sign) (float y-velocity)))))))
           first
           (.indexOf velocities)
           (nth directions)))

(defn dont-overlap-tile
  [{:keys [x y x-change y-change]}
   {:keys [width height]}
   tiled-map]
  (let [old-x (- x x-change)
        old-y (- y y-change)
        touching-x? (tiles/touching-tile? tiled-map "walls" x old-y width height)
        touching-y? (tiles/touching-tile? tiled-map "walls" old-x y width height)]
    (when (or touching-x? touching-y?)
      (cond-> {:x-change 0 :y-change 0}
              touching-x?
              (assoc :x-velocity 0 :x old-x)
              touching-y?
              (assoc :y-velocity 0 :y old-y)))))

(defn move
  [[x-velocity y-velocity]
   {:keys [x y] :as character}
   {:keys [delta-time] :as game}]
  (let [x-change (-> (* x-velocity delta-time)
                     (max min-movement-per-frame)
                     (min max-movement-per-frame))
        y-change (-> (* y-velocity delta-time)
                     (max min-movement-per-frame)
                     (min max-movement-per-frame))
        character (assoc character
                    :x-change x-change
                    :y-change y-change)]
    (if (or (not= 0 x-change) (not= 0 y-change))
      (assoc character
        :x-velocity (decelerate x-velocity)
        :y-velocity (decelerate y-velocity)
        :x (+ x x-change)
        :y (+ y y-change))
      character)))

(defn animate
  [{:keys [x-velocity y-velocity moves] :as character}
   health
   direction
   {:keys [total-time]}
   animations]
  (or (when (<= health 0)
        {:current-image (get-in character [:deads direction])})
      (when-let [{:keys [kind]} (first animations)]
        {:current-image (get (get character kind) direction)})
      (when-let [direction (get-direction x-velocity y-velocity)]
        {:current-image
         (when (or (not= x-velocity 0)
                   (not= y-velocity 0))
           (let [images (direction moves)
                 cycle-time (mod total-time (* animation-secs (count images)))]
             (nth images (int (/ cycle-time animation-secs)))))
         :direction direction})
      {:current-image (get-in moves [direction 0])}))

