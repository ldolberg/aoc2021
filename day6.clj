(require '[clojure.string :as str])
(def input (map read-string (str/split (slurp "inputs/day6") #",")))
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