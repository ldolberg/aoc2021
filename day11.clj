(require '[clojure.string :as str])
(require '[clojure.set :as clset])
(def input
  (->> (slurp "inputs/day11")
       str/split-lines
       (map #(str/split %1 #""))
       (map #(map read-string %1))
       (map vec)
       vec))

(def scores-input {})

(def max_x 10)
(def max_y 10)

(defn ngb [p]
  ;; Computes adjacent cells for a given point
  (let [x (first p)
        y (second p)
        directions [[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 -1] [-1 0] [-1 1]]]
    ;; (println directions)
    (->> directions
         (map #(list (+ x (first %)) (+ y (second %))))
        ;;  (println)
         (filter #(and (>= (first %) 0)
                       (>= (second %) 0)
                       (< (first %) max_x)
                       (< (second %) max_y)))
         set)))
  
(defn update-score [p scores]
  (update-in scores p #(mod (+ 1 %) 10)))

(defn score [p scores]
  (get-in scores p))



(defn update-octopus [p scores flashed]
  (loop [candidates [p] 
         loop-scores scores 
         loop-flashed flashed]
    (if (empty? candidates)
      (hash-map :scores loop-scores, :flashed loop-flashed)
      (let [current (peek candidates)
            ngbs (ngb current)
            updated-score (if (not (contains? loop-flashed current))
                            (update-score current loop-scores)
                            loop-scores)
            flashes?  (if (not (contains? loop-flashed current))
                        (zero? (score current updated-score))
                        false)
                               
            updated-flashed (if flashes?
                              (conj loop-flashed current)
                              loop-flashed)
            new-candidates (if flashes?
                             (reduce #(conj %1 %2) (pop candidates) (clset/difference ngbs updated-flashed))
                             (pop candidates))]
        (recur  new-candidates updated-score updated-flashed)))))



(defn update-round [scores]
  (reduce #(update-octopus %2 (:scores %1) (:flashed %1))
          (hash-map :scores scores, :flashed #{})
          (mapcat #(map (fn [x] [x %]) (range 10)) (range 10))))


(println
 (loop [iter 100 flashes 0 scores input]
   (if (zero? iter)
     
     #{:flashes flashes, :scores scores}
     
     (let [new-scores (update-round scores)]
       (recur (dec iter) 
              (+ flashes (count (:flashed new-scores))) 
              (:scores new-scores))))))

;Part II ez-af
(defn isdone [scores]
  (reduce #(and %1 (zero? (score %2 scores)))
          true
          (mapcat #(map (fn [x] [x %]) (range 10)) (range 10))))


(time (loop [done false steps 0 scores input]
  (if done
    steps
    (let [new-scores (update-round scores)
          new-done (isdone (:scores new-scores))]
      (recur new-done (inc steps) (:scores new-scores))))))