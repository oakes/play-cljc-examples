(ns basic-bird.move
  (:require [basic-bird.utils :as utils]
            [basic-bird.entities :as e]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(def ^:const damping 0.1)
(def ^:const max-velocity 1000)
(def ^:const max-jump-velocity (* max-velocity 8))
(def ^:const deceleration 0.7)
(def ^:const gravity 500)
(def ^:const animation-secs 0.2)

(defn decelerate
  [velocity]
  (let [velocity (* velocity deceleration)]
    (if (< (math abs velocity) damping)
      0
      velocity)))

(defn get-x-velocity
  [pressed-keys {:keys [x-velocity]}]
  (cond
    (contains? pressed-keys :left)
    (* -1 max-velocity)
    (contains? pressed-keys :right)
    max-velocity
    :else
    x-velocity))

(defn get-y-velocity
  [pressed-keys {:keys [y-velocity can-jump?]}]
  (cond
    (and can-jump? (contains? pressed-keys :up))
    (* -1 max-jump-velocity)
    :else
    y-velocity))

(defn get-direction
  [{:keys [x-velocity direction]}]
  (cond
    (> x-velocity 0) :right
    (< x-velocity 0) :left
    :else
    direction))

(defn get-image-key [total-time player]
  (let [image-keys (-> player :images keys sort vec)
        cycle-time (mod total-time (* animation-secs (count image-keys)))]
    (nth image-keys (int (/ cycle-time animation-secs)))))

