(ns super-koalio.tile
  (:require [clojure.data.xml :as xml]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]
            [expound.alpha :as expound]
            #?(:clj [clojure.java.io :as io])))

#?(:clj (defmacro read-tiled-map [fname]
          (slurp (io/resource (str "public/" fname)))))

(defn- str->num [s]
  #?(:clj (Double/parseDouble s)
     :cljs (let [n (js/parseFloat s)]
             (if (= n js/NaN)
               false
               n))))

(defn- parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

(defn- record->map [r]
  (walk/postwalk
    (fn [x]
      (if (record? x)
        (into {} x)
        x))
    r))

;; attrs

(s/def ::version (s/and string? str->num))
(s/def ::name string?)

;; tileset

(s/def :tileset/tag #{:tileset})
(s/def :tileset/attrs (s/keys :req-un [::name]))
(s/def ::tileset (s/keys :req-un [:tileset/tag :tileset/attrs]))

;; layer

(s/def :layer/tag #{:layer})
(s/def :layer/attrs (s/keys :req-un [::name]))
(s/def ::layer (s/keys :req-un [:layer/tag :layer/attrs]))

;; map

(s/def :map/tag #{:map})
(s/def :map/attrs (s/keys :req-un [::version]))
(s/def :map/content (s/coll-of (s/or
                                 :tileset ::tileset
                                 :layer ::layer
                                 :string string?)))
(s/def ::map (s/keys :req-un [:map/tag :map/attrs :map/content]))

;; parse-map

(defn parse-map [tiled-map]
  (let [parsed #?(:clj  (xml/parse (java.io.StringReader. tiled-map))
                  :cljs (xml/parse-str tiled-map))
        parsed (record->map parsed)]
    (println (parse ::map (into {} parsed)))))

