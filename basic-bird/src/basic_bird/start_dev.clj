(ns basic-bird.start-dev
  (:require [basic-bird.start :as start]
            [basic-bird.core :as c]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.alpha :as s]
            [play-cljc.gl.core :as pc])
  (:import [org.lwjgl.glfw GLFW]
           [basic_bird.start Window]))

(defn start []
  (st/instrument)
  (let [window (start/->window)
        game (pc/->game (:handle window))]
    (start/start game window)))

(defn -main []
  (start))

