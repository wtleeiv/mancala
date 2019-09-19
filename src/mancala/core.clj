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
   :game-over false
   :player-to-move 0
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
  [game]
  (let [player (:player-to-move game)
        opponents (filter #(not= player %) (:players game))
        next-opponents (filter #(> % player) opponents)
        prev-opponents (filter #(< % player) opponents)]
    (concat next-opponents prev-opponents)))

(defn mancala-round-trip
  "accessors for entire board, in order,
  beginning with player's mancala"
  [game player]
  (let [initial-well (dec (:num-wells game))]
    (cycle (cons (mancala-accessor player)
                 (concat (mapcat #(well-accessors % initial-well)
                                 (sorted-opponents game))
                         (well-accessors player initial-well))))))

(defn move-seq
  [game player well pieces]
  (take pieces (concat (well-accessors player (dec well))
                       (mancala-round-trip game player))))

(defn place-pieces
  [board wells]
  (reduce (fn [board well-acc]
            (let [well-contents (get-in board well-acc)]
              (assoc-in board well-acc (inc well-contents))))
          board
          wells))

;; TODO  player not needed if :player-to-move w/in game

(defn move-well
  [well-accessor]
  (last well-accessor))

(defn move-owner
  [well-accessor]
  (first well-accessor))

(defn move-ends-in-mancala
  [last-move]
  (= (move-well last-move) :mancala))

(defn update-player-to-move
  [game]
  (let [next-opponent (first (sorted-opponents game))]
    (assoc game :player-to-move next-opponent)))

(defn linked-wells
  [game last-move]
  (let [linked-well (- (:num-wells game)
                       (move-well last-move)
                       1)
        opponents (sorted-opponents game)]
    (map #(well-accessor % linked-well)
         opponents)))

(defn capture-pieces
  [game last-move]
  (let [all-wells (cons last-move (linked-wells game last-move))
        board (:board game)
        all-pieces (reduce (fn [total well-accessor]
                             (+ total (get-in board well-accessor)))
                           0
                           all-wells)
        purged-wells-board (reduce (fn [b well-acc]
                                     (assoc-in b well-acc 0))
                                   board
                                   all-wells)
        player (:player-to-move game)
        man-acc (mancala-accessor player)
        man-count (get-in purged-wells-board man-acc)
        new-man-count (+ all-pieces man-count)
        updated-board (assoc-in purged-wells-board man-acc new-man-count)]
    (assoc game :board updated-board)))

(defn maybe-capture
  [game last-move]
  (let [well-owner (move-owner last-move)
        well (move-well last-move)]
    (if (and (= well-owner (:player-to-move game))
             (= 1 (well-pieces (:board game) well-owner well)))
      (capture-pieces game last-move)
      game)))

(defn finalize-move
  [game player last-move]
  (if (move-ends-in-mancala last-move)
    game ; no :player-to-move update req'd
    (update-player-to-move (maybe-capture game last-move))))

(defn all-wells-empty
  [wells]
  (not (some #(not= 0 %)
             wells)))

(defn check-game-over
  [game]
  (if (some all-wells-empty
            (map (fn [player]
                   (get-in game [:board player :wells]))
                 (:players game)))
    (assoc game :game-over true)
    game))

(defn update-game
  "handle capture/move again/game over logic"
  [game player last-move]
  (check-game-over (finalize-move game player last-move)))

(defn move
  [game player well]
  (let [pieces (well-pieces (:board game) player well)
        removed-source-well-board (empty-well (:board game) player well)
        moves (move-seq game player well pieces)
        placed-pieces-board (place-pieces removed-source-well-board moves)]
    (update-game (assoc game :board placed-pieces-board)
                 player
                 (last moves))))
