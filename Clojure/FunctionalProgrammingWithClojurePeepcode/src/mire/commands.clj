(ns mire.commands
  (:use [mire rooms util player])
  (:use [clojure.contrib str-utils seq-utils]))

;; Command Functions
  
(defn look "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @*current-room*)
       "\nExits: " (keys @(:exits @*current-room*)) "\n"
       (str-join "\n" (map #(str "There is " % " here.\n")
                           @(:items @*current-room*))))) 

(defn move
  "We gotta get out of this place.  Give a direction"
  [direction]
  (dosync
    (let [target-name ((:exits @*current-room*) (keyword direction))
          target (rooms target-name)]
      (if target
        (do (move-between-refs *player-name*
                               (:inhabitants @*current-room*)
                               (:inhabitants target))
            (ref-set *current-room* target)
            (look))
        "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
    (if (room-contains? @*current-room* thing)
      (do (move-between-refs (keyword thing)
                             (:items @*current-room*)
                             *inventory*)
        (str "You picked up the " thing "."))
      (str "There isn't any " thing " here."))))
      
(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
    (if (carrying? thing)
      (do (move-between-refs (keyword thing)
                             *inventory*
                             (:items @*current-room*))
        (str "You dropped the " thing "."))
      (str "You're not carrying a " thing "."))))      

(defn inventory
  "See what you've got."
  []
  (str-join "\n  " (conj @*inventory* "You are carrying:")))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room"))
    "You don't have the detector"))
  
(defn current-time []
  (str "It is now " (java.util.Date.)))

;; Command Data
  
(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
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