(require '[clojure.string :as str])
(def input (str/split-lines (slurp "/home/lautaro/Documents/Workspace/aoc2021/inputs/day5")))
(def coords (->> input 
             (map #(str/split % #" -> "))
             (map (fn [l] (mapcat #(str/split % #",") l)))
             (map #(map read-string %))))
    ;; (partition 2)
     

(def space {})

(defn  update-space [space c]
    (if (get space c nil)
        (update space c inc)
        (assoc space c 1)))
    


(defn abs [x]
    (if (> x 0) x (- x)))



(defn coord-path [[a b c d]]
    (let [
          orig_x (min a c)
          orig_y (min b d)
          dest_x (max a c)
          dest_y (max b d)]
          
        (if (= (abs (- a c)) (abs (- b d))) 
            (partition 2 (into [c d] (mapcat #(vector (if (> a c) (- a %) (+ a %)) (if (> b d) (- b %) (+ b %)) ) (range (abs (- a c))))))
            (if (or (= a c) (= b d))
                (if (= a c)
                    (partition 2 (into [a dest_y] (mapcat #(conj [a] %) (range orig_y dest_y))))
                    (partition 2 (into [dest_x b] (mapcat #(conj (vector %) b) (range orig_x dest_x)))))
                 
                []))))
    

(defn parse-coords [coords]
    (->> coords
        (mapcat coord-path)
        (reduce #(update-space %1 %2) space)
        (filter (fn [x] (>= (val x) 2)))
        count))
        

(parse-coords coords)