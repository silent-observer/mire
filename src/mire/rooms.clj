(ns mire.rooms
  (:require [mire.generator :as gen]
            [mire.count-map :as cm]))

(def rooms (ref {}))

(defn convert-room [room passage-keys]
  (let [relevant-keys (filter #(= (:id room) (get % 2)) passage-keys)
        exits-with-keys 
        (mapcat (fn [[dir to]]
                 (reduce 
                  (fn [old k] (if (and (= (first k) dir))
                                [[dir [to (ref (k 3)) (ref #{})]]] old)) 
                  [[dir [to (ref '()) (ref #{})]]]
                  relevant-keys)) (:exits room))]
       {:id (:id room)
        :name (:id room)
        :desc (:desc room)
        :exits (ref (into {} exits-with-keys))
        :items (ref (cm/to-count-map (:keys room)))
        :inhabitants (ref #{})
        :chests (mapv (fn [[gold codes]] 
                          [(ref :closed) gold codes]) (:chests room))
        :notes (:notes room)}))
         
       

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room)
            :exits (ref (:exits room))
            :items (ref (or (:items room) #{}))
            :inhabitants (ref #{})}})))

(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [rooms]
  (dosync
   (let [[generated-rooms keys-data] (gen/generate-full) _ (println "generated-rooms=" generated-rooms)]
    (reduce (fn [prev room] (conj prev {(:id room) (convert-room room keys-data)})) rooms generated-rooms))))

(defn add-rooms
  "Look through all the files in a dir for files describing rooms and add
  them to the mire.rooms/rooms map."
  []
  (dosync
   (alter rooms load-rooms)))

(defn room-contains?
  [room thing]
  (cm/has? @(:items room) (keyword thing)))
