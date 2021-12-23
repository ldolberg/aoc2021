(require '[clojure.string :as str])
(def input
  (->> (slurp "inputs/day9.sample")
       str/split-lines
       (map #(str/split %1 #""))
       (map #(map read-string %1))))

(defn lower-nb [x y i]
  (println i)
  (cond (and (> i 0) (< i (- (count x) 2)))
        (and (< (nth x i) (nth x (dec i)))
             (< (nth x i) (nth x (inc i)))
             (< (nth x i) (nth y i)))
        (= i 0)
        (and (< (nth x i) (nth x (inc i)))
             (< (nth x i) (nth y i)))
        (= i (dec (count x)))
        (and (< (nth x i) (nth x (dec i)))
             (< (nth x i) (nth y i)))))

(defn low-points [x y c]
;;   (println x)
  (->>
   (range (count x))
   (filter (cond (nil? y) #(lower-nb x c %1)
                 (nil? c) #(lower-nb x y %1)
                 :else #(and (lower-nb x c %1) (lower-nb x y %1))))
   (map #(nth x %1))))

(defn solve [input]
  (reduce #(conj %1 (cond (= %2 0) (low-points (nth input %2)
                                               nil
                                               (nth input (inc %2)))
                          (= %2 (dec (count input))) (low-points (nth input %2)
                                                                 (nth input (dec %2))
                                                                 nil)
                          :else (low-points (nth input %2)
                                            (nth input (dec %2))
                                            (nth input (inc %2))))) [] (range (count input))))

(->> input
     solve
     (map #(map inc %))
     (map #(reduce + %))
     (apply +)
     println)

(println (low-points (first input) nil (second input)))

(println (low-points (second input) (first input) (nth input 3)))