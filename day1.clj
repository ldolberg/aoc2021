;; (load "day1")
(require '[clojure.string :as str])
(def input (map read-string (str/split-lines (slurp "inputs/day1"))))
(time (println (count input)))

;; Slow solution
(defn part_one [input]
  (loop [sonar input counter 0]
    (let [x (first sonar) y (second sonar)]
      (if (nil? y)
        counter
        (recur (rest sonar) (+ counter (if (> y x) 1 0)))))))


(time (println (part_one input)))


;; Fast Solution

(time (->>
       (interleave input (rest input))
       (partition 2)
       (filter #(< (first %) (second %)))
       count
       println))

(println "Second Part")


;; Slow Solution

(defn slide [tseq]
  (loop [elems tseq res []]
    (if (< (count elems) 3)
      res
      (recur (rest elems) (concat res (take 3 elems))))))


(def input2 (->>
             input
             slide
             (partition 3)
             (map #(apply + %))))


(time (println (part_one input2)))


;; Fast Solution
(defn zip_lag [x]
  (interleave x (rest x)))


(time (->>
       input
       (partition 3 1) ;; Generates windows of length 3
       (map #(apply + %)) ;; Sums up windows
       zip_lag ;; Zips it self and lagged 1
       (partition 2) ;; Groups each window with next
       (filter #(< (first %) (second %))) ;; Filter (A,B) => B > A
       count
       println))

