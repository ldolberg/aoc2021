(require '[clojure.string :as str])
(def input (str/split (slurp "inputs/day4") #"\n\n"))

(def numbers (map read-string (str/split (first  input) #",")))

(def boards_raw
  (->> (rest input)
       (map #(str/split % #"\s"))
       (map #(filter not-empty %))
       (map #(map read-string %))))

(defn make-board [board_raw]
  {:index (apply assoc {} (interleave board_raw (range 25)))
   :hits (apply assoc {} (interleave board_raw (repeat 25 0)))
   :rows {0 0 1 0 2 0 3 0 4 0}
   :columns {0 0 1 0 2 0 3 0 4 0}})

(defn update-board [board n]
    ;; (println n)
  (if-let [position_at (get (:index board) n)]
    (let [row (int (/ position_at 5))
          col (int (mod position_at 5))
          updated_hits (update board :hits #(update % n inc))
          updated_rows (update updated_hits :rows #(update % row inc))
          updated_cols (update updated_rows :columns #(update % col inc))]
      updated_cols)
    board))

(defn is-winner? [board]
  (or (some #(= % 5) (vals (get board :rows)))
      (some #(= % 5) (vals (get board :columns)))))

(def bingo (map make-board boards_raw))

(defn winners [k] (loop [bingo_night bingo balls numbers winners []]
                    (let [run (map #(update-board % (first balls)) bingo_night)
                          winner (filter is-winner? run)
                          winners (if (not-empty winner) (into winners winner) winners)]
                      (if (or (empty? (rest balls)) (= (count winners) k))
                        {:board winners :ball (first balls)}
                        (recur (filter #(not (is-winner? %)) run) (rest balls) winners)))))

(def first_winner (winners 1))
(def last_winner (winners 100))

(defn score-board [board_solution]
  (apply + (map key (filter #(= 0 (get (:hits (last (:board board_solution))) (key %))) (:hits (last (:board board_solution)))))))

(println "Part 1" (* (:ball first_winner) (score-board first_winner)))

(println "Part 2" (* (:ball last_winner) (score-board last_winner)))