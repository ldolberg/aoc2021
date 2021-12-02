(require '[clojure.string :as str])
(def input (str/split-lines (slurp "inputs/day2")))

(defn direction-h [i]
  (let [direction (first (str/split i #" "))
        n (Integer/parseInt (second (str/split i #" ")))]
      (if (some #(= % direction) ["up", "down"])
        (* n (if (= direction "up") -1 1))
        0
      )      
  )
)


(defn direction-d [i]
  (let [direction (first (str/split i #" "))
        n (Integer/parseInt (second (str/split i #" ")))]
      (if (some #(= % direction) ["forward", "backward"])
        (* n (if (= direction "forward") 1 -1))
        0
      )      
  )
)

(defn part_one [input]
  (loop [elems input horizontal 0 depth 0]
    (if (empty? elems)
      (* horizontal depth)
      (recur (rest elems) 
             (+ horizontal (direction-h (first elems))) 
             (+ depth (direction-d (first elems)))))))     

(->> input
  part_one
  println)


(defn part_two [input]
  (loop [elems input horizontal 0 depth 0 aim 0]
    (if (empty? elems)
      (* horizontal depth)
      (recur (rest elems) 
             (+ horizontal (* aim (direction-d (first elems)))) 
             (+ depth (direction-d (first elems)))
             (+ (direction-h (first elems)) aim)))))

(->> input
  part_two
  println)
