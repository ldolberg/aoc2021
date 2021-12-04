(require '[clojure.string :as str])
(def input (str/split-lines (slurp "inputs/day3")))
(def nlines (count input))

(defn parse_line [in]
  (map read-string (str/split in #""))
)

(defn find_most_common_based_on_first_digit [k elems]
  (let [freq (reduce
      (fn [a b]
        (println a b (nth b k))
        (if (= (nth b k) 0) 
          {:zero (inc (:zero a)) :one (:one a)}
          {:zero (:zero a) :one (inc (:one a))}))
    {:zero 0 :one 0}
    elems)])   
)

(defn most_frequent_key_by_pos [x n] 
  (key (apply max-key val (frequencies (map #(nth % n) x)))))

(defn least_frequent_key_by_pos [x n] 
  (key (apply min-key val (frequencies (map #(nth % n) x)))))

(def a
  (Integer/parseInt (->> (range 12)
    (map #(most_frequent_key_by_pos (map parse_line input) %))
    (reduce conj [])
    (str/join "")) 2))

(def b 
  (Integer/parseInt (->> (range 12)
    (map #(least_frequent_key_by_pos (map parse_line input) %))
    (reduce conj [])
    (str/join "")) 2))



(println "Part 1")
(println (* a b))

(defn filter-most-frequents-by-index [k elems]
  (let [mf (most_frequent_key_by_pos elems k)]
  (filter 
    #(= (nth % k) mf)
    elems )))

(defn filter-least-frequents-by-index [k elems]
  (let [mf (least_frequent_key_by_pos elems k)]
  (filter 
    #(= (nth % k) mf)
    elems )))


(def a2
     (loop [elems (map parse_line input) idx 0]
      (if (= idx 12)
        elems
        (recur (filter-most-frequents-by-index idx elems) (inc idx)))))


(def b2
  (loop [elems (map parse_line input) idx 0]
    (if (= idx 12)
      elems
      (recur (filter-least-frequents-by-index idx elems) (inc idx)))))

(println "Part 2")
(println 
  (reduce * 
    (map #(Integer/parseInt % 2) 
      (map #(str/join "" %) 
        (map first 
          [a2 b2])))))