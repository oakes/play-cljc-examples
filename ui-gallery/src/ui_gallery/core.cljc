(ns ui-gallery.core
  (:require [ui-gallery.utils :as utils]
            [ui-gallery.chars :as chars]
            [play-cljc.gl.core :as c]
            [play-cljc.transforms :as t]
            [play-cljc.gl.text :as text]
            [play-cljc.instances :as i]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])
            #?(:clj [ui-gallery.text :refer [load-font-clj]]))
  #?(:cljs (:require-macros [ui-gallery.text :refer [load-font-cljs]])))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}
                       :counter 0}))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  ;; load font
  (#?(:clj load-font-clj :cljs load-font-cljs) :roboto
     (fn [{:keys [data]} baked-font]
       (let [font-entity (text/->font-entity game data baked-font)
             compiled-font-entity (c/compile game font-entity)
             static-entity (c/compile game (text/->text-entity game compiled-font-entity "Hello, world!"))
             dynamic-entity (c/compile game (i/->instanced-entity font-entity))]
         (swap! *state assoc
                :font-entity font-entity
                :static-entity static-entity
                :dynamic-entity dynamic-entity)))))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [(/ 173 255) (/ 216 255) (/ 230 255) 1] :depth 1}})

(defn run [game]
  (let [game-width (utils/get-width game)
        game-height (utils/get-height game)
        {:keys [font-entity
                static-entity
                dynamic-entity
                counter]} @*state]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))
    ;; render the font
    (when (and static-entity dynamic-entity)
      (c/render game (-> static-entity
                         (t/project game-width game-height)
                         (t/scale (:width static-entity) (:height static-entity))))
      (let [text ["Counter:" (str counter)]]
        (c/render game (-> (reduce
                             (partial apply chars/assoc-char)
                             dynamic-entity
                             (for [line-num (range (count text))
                                   char-num (range (count (nth text line-num)))
                                   :let [ch (get-in text [line-num char-num])]]
                               [line-num char-num (chars/crop-char font-entity ch)]))
                           (t/project game-width game-height)
                           (t/translate 0 200))))))
  (swap! *state update :counter inc)
  ;; return the game map
  game)

