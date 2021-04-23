(ns dungeon-crawler.start-dev
  (:require [dungeon-crawler.start]
            [clojure.spec.test.alpha :as st]))

(st/instrument)
(st/unstrument 'odoyle.rules/insert) ;; don't require specs for attributes

