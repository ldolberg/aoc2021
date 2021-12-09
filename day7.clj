(require '[clojure.string :as str])
(def input (map read-string (str/split (slurp "inputs/day7") #",")))
;; (def input (list 16 1 2 0 4 2 7 1 2 14))

(defn abs [x] (if (> x 0) x (* -1 x)))

(defn solve [pos_list]
  (->>
   pos_list
   (map (fn [x] 
          (reduce #(+ %1
                      (/ (* (inc (abs (- x %2)))
                            (abs (- x %2)))
                         2))
                  0
                  pos_list)))
   (apply min)))

(println (solve input))
(println (solve (range (apply max input))))
