(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown task:" task-name)
  (System/exit 1))

(require '[figwheel.main :as figwheel])

(defmethod task nil
  [_]
  (figwheel/-main "--build" "dev"))

(require '[basic-bird.start-dev])

(defmethod task "native"
  [_]
  (basic-bird.start-dev/start))

(defmethod task "repl"
  [_]
  (clojure.main/repl :init #(doto 'basic-bird.start-dev require in-ns)))

(task *command-line-args*)
