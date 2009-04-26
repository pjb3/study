(ns mire
  (:use [clojure.contrib server-socket duck-streams]))
  
(defn mire-handle-client [in out]
  (binding [*in* (reader in)
            *out* (writer out)]
          (loop []
            (println (read-line))
            (recur))))
            
(def server (create-server 3333 mire-handle-client))