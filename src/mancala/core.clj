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

(defn default-board
  []
  (make-board 2 6 4))

(defn well-accessor
  [player well]
  [player :wells well])

(defn well-pieces
  [board player well]
  (get-in board (well-accessor player well)))

(defn empty-well
  [board player well]
  (assoc-in board (well-accessor) 0))

(defn board-seq
  [game player well]
  (let [pieces (well-pieces game player well)]
    (take
     pieces
     (repeatedly
      (fn [game player well]
        )))))
