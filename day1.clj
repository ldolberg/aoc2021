(def input (slurp "input/day1"))
;; (println input)



(defn part_one [input]
  (loop [sonar input counter 0]
    (let [x (first sonar) y (second sonar)]
      (if (nil? y) 
        counter 
        (recur (rest sonar) (+ counter (if (> y x) 1 0)))
    ))
))