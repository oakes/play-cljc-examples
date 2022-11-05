(ns super-koalio.start-dev
  (:require [super-koalio.start :as start]
            [super-koalio.core :as c]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.alpha :as s]
            [play-cljc.gl.core :as pc])
  (:import [org.lwjgl.glfw GLFW]
           [super_koalio.start Window]))

(defn start []
  (st/instrument)
  (st/unstrument 'odoyle.rules/insert) ;; don't require specs for attributes
  (let [window (start/->window)
        game (pc/->game (:handle window))]
    (start/start game window)))

(defn -main []
  (start))

