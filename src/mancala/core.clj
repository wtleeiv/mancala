(ns mancala.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn make-board
  [num-players num-wells num-pieces-per-well]
  (reduce (fn [board player-number]
            (assoc board
                   player-number
                   {:mancala 0
                    :wells (into []
                                 (take num-wells
                                       (repeat num-pieces-per-well)))}))
          {}
          (range num-players)))

(defn make-game
  [num-players num-wells num-pieces-per-well]
  {:num-players num-players
   :players (into [] (range num-players))
   :num-wells num-wells
   :board (make-board num-players num-wells num-pieces-per-well)})

(defn default-game
  []
  (make-game 2 6 4))

(defn well-accessor
  [player well]
  [player :wells well])

(defn well-pieces
  [board player well]
  (get-in board (well-accessor player well)))

(defn empty-well
  [board player well]
  (assoc-in board (well-accessor player well) 0))

(defn well-accessors
  [player initial-well]
  (map (partial well-accessor player) (range initial-well -1 -1)))

(defn mancala-accessor
  [player]
  [player :mancala])

(defn sorted-opponents
  [game player]
  (let [opponents (filter #(not= player %) (:players game))
        next-opponents (filter #(> % player) opponents)
        prev-opponents (filter #(< % player) opponents)]
    (concat next-opponents prev-opponents)))

(defn mancala-round-trip
  [game player]
  (let [initial-well (dec (:num-wells game))]
    (cycle (cons (mancala-accessor player)
                 (concat (mapcat #(well-accessors % initial-well)
                                 (sorted-opponents game player))
                         (well-accessors player initial-well))))))

(defn board-seq
  [game player well]
  (let [pieces (well-pieces (:board game) player well)]
    (take pieces (concat (well-accessors player (dec well))
                         (mancala-round-trip game player)))))

(defn place-pieces
  [board wells]
  (reduce (fn [board well-acc]
            (let [well-contents (get-in board well-acc)]
              (assoc-in board well-acc (inc well-contents))))
          board
          wells))

(defn move
  [game player well]
  (let [board (place-pieces (:board game) (board-seq game player well))]
    (empty-well board player well)))
