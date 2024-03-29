(ns dungeon-crawler.utils
  #?(:clj (:require [clojure.java.io :as io]))
  #?(:clj (:import [java.nio ByteBuffer]
                   [org.lwjgl.glfw GLFW]
                   [org.lwjgl.system MemoryUtil]
                   [org.lwjgl.stb STBImage]
                   [javax.sound.sampled AudioSystem Clip])))

(defn get-image [fname callback]
  #?(:clj  (let [is (io/input-stream (io/resource (str "public/" fname)))
                 ^bytes barray (with-open [out (java.io.ByteArrayOutputStream.)]
                                 (io/copy is out)
                                 (.toByteArray out))
                 *width (MemoryUtil/memAllocInt 1)
                 *height (MemoryUtil/memAllocInt 1)
                 *components (MemoryUtil/memAllocInt 1)
                 direct-buffer (doto ^ByteBuffer (ByteBuffer/allocateDirect (alength barray))
                                 (.put barray)
                                 (.flip))
                 decoded-image (STBImage/stbi_load_from_memory
                                 direct-buffer *width *height *components
                                 STBImage/STBI_rgb_alpha)
                 image {:data decoded-image
                        :width (.get *width)
                        :height (.get *height)}]
             (MemoryUtil/memFree *width)
             (MemoryUtil/memFree *height)
             (MemoryUtil/memFree *components)
             (callback image))
     :cljs (let [image (js/Image.)]
             (doto image
               (-> .-src (set! fname))
               (-> .-onload (set! #(callback {:data image
                                              :width image.width
                                              :height image.height})))))))

(defn get-size [game]
  #?(:clj  (let [*width (MemoryUtil/memAllocInt 1)
                 *height (MemoryUtil/memAllocInt 1)
                 _ (GLFW/glfwGetFramebufferSize ^long (:context game) *width *height)
                 w (.get *width)
                 h (.get *height)]
             (MemoryUtil/memFree *width)
             (MemoryUtil/memFree *height)
             [w h])
     :cljs [(-> game :context .-canvas .-clientWidth)
            (-> game :context .-canvas .-clientHeight)]))

(defn get-width [game]
  (first (get-size game)))

(defn get-height [game]
  (second (get-size game)))

(def sounds
  (reduce
    (fn [m file-name]
      (assoc m file-name
        #?(:clj (doto (AudioSystem/getClip)
                  (.open (AudioSystem/getAudioInputStream (io/resource (str "public/" file-name)))))
           :cljs (doto (js/Audio.)
                   (-> .-src (set! file-name))))))
    {}
    ["monsterhurt.wav" "playerhurt.wav" "death.wav"]))

(defn play-sound! [file-name]
  #?(:clj (future
            (doto (get sounds file-name)
              (.setFramePosition 0)
              .start))
     :cljs (.play (get sounds file-name))))

