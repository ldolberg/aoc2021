(require '[clojure.string :as str])
(def input (map read-string (str/split (slurp "inputs/day6") #",")))

(defn evolve-lanternfish [state]
  (reduce (fn [acc, p] 
            (if (zero? p)
              (conj (conj acc 6) 8)
              (conj acc (dec p))))
          []
          state))

;; (println (evolve-lanternfish input))

(defn evolve-n-days [days] (loop [n 0 state input]
  ;; (println n state)
  (if (zero? (- n days))
      state
      (recur (inc n) (evolve-lanternfish state))
  )))

(time (println (count input)))

(println "Part I: Snail mail")
(time (println (count (evolve-n-days 80))))

;; This method dosen't scale...oopsy
;; Lets model the population as a hash-map


(def initial-freq (frequencies input))

(defn evolve-freq [freqs] 
  (let [ updated-freq (apply assoc {} (interleave (map #(mod (dec %) 9) (keys freqs)) (vals freqs)))]
    (if (get updated-freq 6)
      (update updated-freq  6 (partial + (get freqs 0 0)))
      (assoc updated-freq 6 (get freqs 0 0)))))

(defn evolve-n-days-freqs [days]
  (loop [n 0 state initial-freq]
    (if (zero? (- n days))
        state
        (recur (inc n) (evolve-freq state))
    )))

(println "Part I: Blazing Fast")

(time (->>
  (evolve-n-days-freqs 80)
  (map val )
  (apply +)
  println))

(println "Part II: Blazing Fast")
(time (->>
  (evolve-n-days-freqs 256)
  (map val )
  (apply +)
  println))