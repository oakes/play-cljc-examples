(ns basic-bird.session
  (:require [basic-bird.move :as move]
            [basic-bird.entities :as e]
            [odoyle.rules :as o]
            [clojure.spec.alpha :as s]))

(defonce *session (atom nil))
(defonce *reload? (atom false))

(def rules
  (o/ruleset
    {::window
     [:what
      [::window ::width width]
      [::window ::height height]]

     ::keys
     [:what
      [::keys ::pressed pressed]]

     ::mouse
     [:what
      [::mouse ::x x]
      [::mouse ::y y]
      [::mouse ::button button]]

     ::player
     [:what
      [::player ::e/x x]
      [::player ::e/y y]
      [::player ::e/x-change x-change]
      [::player ::e/y-change y-change]
      [::player ::e/x-velocity x-velocity]
      [::player ::e/y-velocity y-velocity]
      [::player ::e/width width]
      [::player ::e/height height]
      [::player ::e/images images]
      [::player ::e/image-key image-key]
      [::player ::e/current-image current-image]
      [::player ::e/can-jump? can-jump?]
      [::player ::e/direction direction]
      :then
      ;; create a derived "all" fact that contains
      ;; all the fields above in a single map
      (o/insert! ::player ::all match)]

     ::current-image
     [:what
      [::window ::width width]
      [::player ::e/image-key image-key]
      [::player ::e/images images]
      :then
      (when-let [player-image (image-key images)]
        (let [player-width (/ width 10)
              player-height (* player-width (/ (:height player-image) (:width player-image)))]
          (o/insert! ::player {::e/width player-width
                               ::e/height player-height
                               ::e/current-image player-image})))]

     ::move-player
     [:what
      [::time ::delta delta-time]
      [::keys ::pressed pressed]
      [::player ::all player {:then false}]
      :then
      (let [x-velocity (move/get-x-velocity pressed player)
            y-velocity (+ (move/get-y-velocity pressed player) move/gravity)
            x-change (* x-velocity delta-time)
            y-change (* y-velocity delta-time)]
        (when (or (not= 0 x-change) (not= 0 y-change))
          (o/insert! ::player
            {::e/x-velocity (move/decelerate x-velocity)
             ::e/y-velocity (move/decelerate y-velocity)
             ::e/x-change x-change
             ::e/y-change y-change
             ::e/x (+ (:x player) x-change)
             ::e/y (+ (:y player) y-change)
             ::e/can-jump? (if (neg? y-velocity) false (:can-jump? player))})))]
     
     ::prevent-move-left
     [:what
      [::player ::all player]
      :when
      (< (:x player) 0)
      :then
      (let [old-x (- (:x player) (:x-change player))
            left-edge 0]
        (o/insert! ::player
                   {::e/x-velocity 0
                    ::e/x (max old-x left-edge)}))]
     
     ::prevent-move-right
     [:what
      [::window ::width width]
      [::player ::all player]
      :when
      (> (:x player) (- width (:width player)))
      :then
      (let [old-x (- (:x player) (:x-change player))
            right-edge (- width (:width player))]
        (o/insert! ::player
                   {::e/x-velocity 0
                    ::e/x (min old-x right-edge)}))]
     
     ::prevent-move-down
     [:what
      [::window ::height height]
      [::player ::all player]
      :when
      (> (:y player) (- height (:height player)))
      :then
      (let [old-y (- (:y player) (:y-change player))
            bottom-edge (- height (:height player))]
        (o/insert! ::player
                   {::e/y-velocity 0
                    ::e/y (min old-y bottom-edge)
                    ::e/can-jump? true}))]

     ::animate-player
     [:what
      [::time ::total total-time]
      [::player ::all player {:then false}]
      :when
      (not= 0 (:x-velocity player))
      (= 0 (:y-velocity player))
      :then
      (o/insert! ::player
                 {::e/image-key (move/get-image-key total-time player)
                  ::e/direction (move/get-direction player)})]}))

(def initial-session
  (reduce o/add-rule (o/->session) rules))

;; when this ns is reloaded, reload the session
(when @*session
  (reset! *reload? true))

;; specs

(s/def ::total number?)
(s/def ::delta number?)

(s/def ::width number?)
(s/def ::height number?)

(s/def ::e/x number?)
(s/def ::e/y number?)
(s/def ::e/x-change number?)
(s/def ::e/y-change number?)
(s/def ::e/x-velocity number?)
(s/def ::e/y-velocity number?)
(s/def ::e/width number?)
(s/def ::e/height number?)
(s/def ::e/can-jump? boolean?)
(s/def ::e/direction #{:left :right})
(s/def ::e/images map?)
(s/def ::e/image-key keyword?)
(s/def ::e/current-image (s/nilable map?))

(s/def ::all map?)

(s/def ::pressed (s/coll-of keyword? :kind set?))
(s/def ::x number?)
(s/def ::y number?)
(s/def ::button (s/nilable keyword?))

