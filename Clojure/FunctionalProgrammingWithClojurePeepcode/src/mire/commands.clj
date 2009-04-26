(ns mire.commands
  (:use [mire rooms])
  (:use [clojure.contrib str-utils seq-utils]))

;; Command Functions
  
(defn look "Get a description of the surrounding environment"
  []
  (str (:desc @*current-room*)
       "\nExits: " (keys @(:exits @*current-room*))
       ".\n"))  

(defn move
  "We gotta get out of this place.  Give a direction"
  [direction]
  (dosync
    (let [target-name ((:exits @*current-room*) (keyword direction))
          target (rooms target-name)]
      (if target
        (do (alter (:inhabitants @*current-room*) disj player-name)
            (alter (:inhabitants target) conj player-name)
            (ref-set *current-room* target)
            (look))
        "You can't go that way."))))
  
(defn current-time []
  (str "It is now " (java.util.Date.)))

;; Command Data
  
(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "look" look})
  
(defn execute
  "Execute a command that is passed to us."
  [input]
  (let [input-words (re-split #"\s+" input)
        command-name (first input-words)
        command (commands command-name)
        args (rest input-words)]
    (if command
      (apply command args)
      "Unknown Command")))    