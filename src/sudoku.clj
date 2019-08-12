(ns sudoku
  (:require [clojure.set :as set]))

(def boxes [#{0 1 2 9 10 11 18 19 20}
            #{3 4 5 12 13 14 21 22 23}
            #{6 7 8 15 16 17 24 25 26}
            #{27 28 29 26 37 38 45 46 47}
            #{30 31 32 39 40 41 48 49 50}
            #{33 34 35 42 43 44 51 52 53}
            #{54 55 56 63 64 65 72 73 74}
            #{57 58 59 66 67 68 75 76 77}
            #{60 61 62 69 70 71 78 79 80}])

(def numbers #{1 2 3 4 5 6 7 8 9})

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

(defn blanks
  "Return sequence of blank positions for mat."
  [sudoku]
  (->> (enumerate sudoku)
       (filter #(= (second %) 0))))

(defn print-sudoku
  "Print sudoku as table."
  [sudoku]
  (doseq [row (range 0 9)]
    (doseq [col (range 0 9)]
      (print (get sudoku (+ col (* row 9))) " "))
    (println)))

(defn candidates
  "Return candidate numbers for given position pos."
  [sudoku pos]
  (set/difference numbers
                  (row sudoku pos)
                  (col sudoku pos)
                  (box sudoku pos)))

(defn solve [sudoku]
  (loop [[state & states] (list {:nums sudoku
                                 :pos (ffirst (blanks sudoku))
                                 :cands (candidates sudoku (ffirst (blanks sudoku)))})]

    (if-not (empty? (:cands state))
      (let [new-mat (assoc (:nums state) (:pos state) (first (:cands state)))
            blank-pos (ffirst (blanks new-mat))]
        (if (nil? blank-pos) new-mat
            (recur (conj states (assoc state :cands (rest (:cands state)))
                         {:nums new-mat
                          :pos (ffirst (blanks new-mat))
                          :cands (candidates new-mat blank-pos)}))))
      (recur states))))
