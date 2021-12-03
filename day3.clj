(require '[clojure.string :as str])
(def input (str/split-lines (slurp "inputs/day3")))
(def nlines (count input))

(defn parse_line [in]
  (map read-string (str/split in #""))
)


(defn add-vec [y x]
  (loop [a x b y res []]
    (let [[xi & x_tail] a [yi & y_tail] b]
      (if (empty? x_tail)
        res
        (recur x_tail y_tail (conj res (+ xi yi)))))))

(defn resolve [x]
  (map #(if (> (/ nlines 2) %) 1 0) x)
)

(->>
input
(map parse_line)
(reduce add-vec)
