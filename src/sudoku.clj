(ns sudoku
  (:require [clojure.set :as set]
            [clojure.string :as s]))

(def boxes [#{0 1 2 9 10 11 18 19 20}
            #{3 4 5 12 13 14 21 22 23}
            #{6 7 8 15 16 17 24 25 26}
            #{27 28 29 36 37 38 45 46 47}
            #{30 31 32 39 40 41 48 49 50}
            #{33 34 35 42 43 44 51 52 53}
            #{54 55 56 63 64 65 72 73 74}
            #{57 58 59 66 67 68 75 76 77}
            #{60 61 62 69 70 71 78 79 80}])

(def numbers #{1 2 3 4 5 6 7 8 9})

(defn parse-sudokus
  "Parse sudokus from file into vector."
  [fname]
  (let [data (slurp fname)
        lines (mapv #(s/replace % #"\s+" "") (drop 1 (s/split data #"Grid \d\d\n")))]
    (vec (for [num-line (map #(s/split % #"") lines)]
           (mapv #(Integer/parseInt %) num-line)))))

(def sudokus (parse-sudokus "resources/sudoku.txt"))

(defn row
  "Return set of row numbers for position pos."
  [sudoku pos]
  (let [pos (* 9 (int (/ pos 9)))]
    (-> (into #{} (subvec sudoku pos (+ pos 9)))
        (disj 0))))

(defn col
  "Return set of column numbers for position pos."
  [sudoku pos]
  (let [column (mod pos 9)]
    (-> (into #{} (take-nth 9 (drop column sudoku)))
        (disj 0))))

(defn box
  "Return set of box numbers for position pos."
  [sudoku pos]
  (let [positions (first (filter #(if (% pos) %) boxes))]
    (->> (map #(get sudoku %) positions)
         (remove #{0})
         (into #{}))))

(defn enumerate
  "Return enumeration vector of pos,val pairs for matrix"
  [sudoku]
  (reduce-kv #(conj %1 [%2 %3]) [] sudoku))

(defn next-blank
  "Return first blank position"
  [sudoku]
  (->> (enumerate sudoku)
       (filter #(zero? (second %)))
       (ffirst)))

(defn candidates
  "Return candidate numbers for given position pos."
  [sudoku pos]
  (seq (set/difference numbers
                       (row sudoku pos)
                       (col sudoku pos)
                       (box sudoku pos))))

(defn solve
  "Solve sudoku by recursion."
  [sudoku]
  (let [blank (next-blank sudoku)]
    (loop [[state & states] (list {:nums sudoku
                                   :pos blank
                                   :cands (candidates sudoku blank)})]
      (if-not (empty? (:cands state))
        (let [new-nums (assoc (:nums state) (:pos state) (first (:cands state)))
              new-blank (next-blank new-nums)]
          (if (nil? new-blank)
            new-nums
            (recur (conj states (assoc state :cands (rest (:cands state)))
                         {:nums new-nums
                          :pos new-blank
                          :cands (candidates new-nums new-blank)}))))
        (recur states)))))

(defn solve-iteration
  "Solve sudoku by iterating. This is used by animation."
  [[state & states]]
  (if-not (empty? (:cands state))
    (let [new-nums (assoc (:nums state) (:pos state) (first (:cands state)))
          blank (next-blank new-nums)]
      (if (nil? blank) {:result new-nums}
          (conj states (assoc state :cands (rest (:cands state)))
                {:nums new-nums
                 :pos blank
                 :cands (candidates new-nums blank)})))
    states))

(defn solve-euler96
  "Project-Euler problem 96 solver."
  []
  (->> (pmap solve sudokus)
       (map #(take 3 %))
       (map #(apply str %))
       (map #(Integer/parseInt %))
       (reduce +)))

(defn -main []
  (println "Euler 96:" (solve-euler96))
  ;; Correct answer: 24702
  (shutdown-agents))
