(ns dungeon-crawler.start-dev
  (:require [dungeon-crawler.start :as start]
            [dungeon-crawler.core :as c]
            [clojure.spec.test.alpha :as st]
            [clojure.spec.alpha :as s]
            [play-cljc.gl.core :as pc])
  (:import [org.lwjgl.glfw GLFW]
           [dungeon_crawler.start Window]))

(defn start []
  (st/instrument)
  (st/unstrument 'odoyle.rules/insert) ;; don't require specs for attributes
  (let [window (start/->window)
        game (pc/->game (:handle window))]
    (start/start game window)))

(defn -main []
  (start))

