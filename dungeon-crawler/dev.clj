(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown task:" task-name)
  (System/exit 1))

(require '[figwheel.main :as figwheel])

(defmethod task nil
  [_]
  (figwheel/-main "--build" "dev"))

(require '[dungeon-crawler.start-dev])

(defmethod task "native"
  [_]
  (dungeon-crawler.start-dev/start))

(task *command-line-args*)
