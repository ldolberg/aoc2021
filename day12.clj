(:require '[clojure.string :as str])
(def test-input "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn parse-input [p]
 (->>
  p
  str/split-lines
  (map #(str/split % #"-"))
  (group-by #(first %))))

(def input (parse-input (slurp "inputs/day12")))

(def graph (parse-input test-input))
(def out-nodes (keys graph))
(def nodes (set (apply concat (reduce concat (vals graph)))))

(defn big-cave? [node]
  (= (str/upper-case node) node))

(map reverse (filter #(big-cave? (first %)) (apply concat (vals graph)))
        
;; [start []]
;; [[A [start] [b [start]]]
;; [[c [start A]] [b [start]]]
;; 
(defn make-path [current-path nodes]
  (reduce #(conj %1 (conj current-path %2)) [] nodes))

(defn neighbors [node]
  ;; if lower case then allows bidiretional edge 
  (map second (get graph node)))

(defn non-visited [visited candidate]
  (filter #(or (big-cave? %) (not (contains? visited %))
                          (neighbors candidate))))

(make-path ["start"] (neighbors "start"))

(defn dfs [node]
  (loop [candidates [[node [node]]] visited #{} paths [node] res []]
    (let [candidate (first (first candidates))
          current-path (second (first candidates))
          new-paths (make-path current-path (non-visited visited candidate))
          new-candidates (concat (rest candidates)
                                 (partition 2 (interleave (non-visited visited candidate) new-paths)))]
      (println current-path)
      (if (or (empty? candidates)
              false)
        (println "hi" current-path res)
        (recur new-candidates (conj visited candidate) new-paths [])))))
      

