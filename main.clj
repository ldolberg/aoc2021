;; (load "day1")
(require '[clojure.string :as str])
(def input (map read-string (str/split-lines (slurp "inputs/day1"))))

(time (println (count input)))

;; Fast solution
(defn part_one [input]
  (loop [sonar input counter 0]
    (let [x (first sonar) y (second sonar)]
      (if (nil? y) 
        counter 
        (recur (rest sonar) (+ counter (if (> y x) 1 0)))
    ))
))
(time (println (part_one input)))

;; Slow Solution

(time (->>
(interleave input (rest input))
(partition 2)
(filter #(< (first %) (second %)))
count
println))


