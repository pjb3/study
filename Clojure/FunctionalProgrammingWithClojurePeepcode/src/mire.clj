(add-classpath (str "file://" (.getParent (java.io.File. *file*)) "/"))

(ns mire
  (:use [mire commands rooms player])
  (:use [clojure.contrib server-socket duck-streams]))

(def port 3333)
(def prompt "> ")
  
(defn- mire-handle-client [in out]
  (binding [*in* (reader in)
            *out* (writer out)]
    
    (print "\nWhat is your name? ")
    (flush)
    
    (binding [*player-name* (read-line)
              *current-room* (ref (rooms :start))
              *inventory* (ref #{})]
      (dosync (alter (:inhabitants @*current-room*) conj *player-name*))
      
      (println (look)) 
      (print prompt) 
      (flush)

      (loop [input (read-line)]
        (println (execute input))
        (print prompt)
        (flush)
        (recur (read-line))))))

(set-rooms "data/rooms")            
(def server (create-server port mire-handle-client))
(println "Mire Server Listening on Port" port)
  

  