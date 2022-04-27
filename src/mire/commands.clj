(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]
            [mire.count-map :as cm]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- move-between-count-maps
  "Move one instance of obj between two count-maps from and to. Must call in a transaction."
  [obj from to]
  (alter from cm/cm-remove obj)
  (alter to cm/cm-add obj))

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*) "(" (:id @player/*current-room*) ")"
       "\nExits: " (apply list (map
                                (fn [[dir [_ k _]]] [dir (if (empty? @k) "open" "closed")]) 
                                @(:exits @player/*current-room*))) 
       "\n\n"
       (if (= (count @(:inhabitants @player/*current-room*)) 1) ""
        (str
          (str/join (map #(str % " is in the room.\n")
                      (disj @(:inhabitants @player/*current-room*) player/*name*))) 
          "\n"))
       (str/join (map #(str "There is " % " here.\n")
                       @(:items @player/*current-room*)))
       (if (nil? (:chest @player/*current-room*)) ""
        (case @(first (:chest @player/*current-room*))
          :closed "There is a closed chest here.\n"
          :open "There is an empty chest here.\n"))
       (if (empty? (:notes @player/*current-room*)) ""
         (if (= (count (:notes @player/*current-room*)) 1)
           "There is a note lying on the floor.\n"
           "There are some notes lying on the floor.\n"))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [[target-name required-keys _] ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if (or target (= target-name -1))
       (if (empty? @required-keys)
        (if target
          (do
            (move-between-refs player/*name*
                              (:inhabitants @player/*current-room*)
                              (:inhabitants target))
            (ref-set player/*current-room* target)
            (look))
          (do
            (doseq [inhabitant (disj (keys player/streams)
                                player/*name*)]
              (binding [*out* (player/streams inhabitant)]
                (println player/*name* " has won!")
                (print player/prompt) (flush)))
            (ref-set rooms/game-over true)
            "You won!"))
            
        
        (str "The door is closed. To open it you need the following keys: " (apply list @required-keys)))
      "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (if (and (player/is-key? thing) (player/carrying-key?)) 
      (str "You're already carrying a key")
      (do (move-between-count-maps (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing ".")))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-count-maps (keyword thing)
                                  player/*inventory*
                                  (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))
       "\nYou also have " @player/*gold* " gold."))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println player/*name* "says:" message)
        (print player/prompt) (flush)))
    (str "You said " message)))

(defn remove-one [thing coll]
  (let [[n m] (split-with #(not= thing %) coll)] (concat n (rest m))))

(defn use-key
  "Use key on the door."
  [thing door]
  (dosync
    (let [[target-name required-keys who-used] ((:exits @player/*current-room*) (keyword door))]
        (cond 
         (not (player/is-key? thing)) (str thing " is not a key.")
         (not (player/carrying? thing)) (str "You're not carrying a " thing ".")
         (not target-name) "There's no door like that."

         (not (some #{(keyword thing)} @required-keys)) 
         (str "The " door " door doesn't need a " thing ".")

         (@who-used player/*name*)
         "You cannot use more than one key on a single door, ask someone else to do it."

         :else (do 
                (alter player/*inventory* cm/cm-remove (keyword thing))
                (alter (second (@(:exits @player/*current-room*) (keyword door)))
                       #(remove-one (keyword thing) %))
                (alter ((@(:exits @player/*current-room*) (keyword door)) 2)
                       #(conj % player/*name*))
                (str "You used the " thing " on the " door " door."))))))

(defn notes
  "Look at the notes."
  []
  (if (empty? (:notes @player/*current-room*)) "There are no notes."
    (if (= (count (:notes @player/*current-room*)) 1)
      (str "The note contains a following number: " 
        (first (:notes @player/*current-room*)))
      (str "The notes contain following numbers: " 
        (:notes @player/*current-room*)))))

(defn chest
  "Try to open the chest."
  [code]
  (dosync
    (let [[status gold codes] (:chest @player/*current-room*)]
        (cond 
         (nil? (:chest @player/*current-room*)) "There's no chest here."
         (= @status :open) "The chest is already opened."
        
         (not (some #{(str code)} codes)) 
         (str "The code didn't work. Maybe try another one?")
        
         :else (do 
                (alter player/*gold* + gold)
                (ref-set status :open)
                (str "You opened the chest. There was " gold " gold there. You now have " @player/*gold* " gold."))))))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "up" (fn [] (move :up)),
               "down" (fn [] (move :down)),
               "exit" (fn [] (move :exit)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "use" use-key
               "notes" notes
               "chest" chest
               "help" help})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
