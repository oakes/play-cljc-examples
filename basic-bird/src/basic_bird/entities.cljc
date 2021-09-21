(ns basic-bird.entities)

(defn ->player []
  {::x 0
   ::y 0
   ::x-change 0
   ::y-change 0
   ::x-velocity 0
   ::y-velocity 0
   ::width 0
   ::height 0
   ::direction :right
   ::can-jump? false
   ::image-key :walk1
   ::images {}
   ::current-image nil})

