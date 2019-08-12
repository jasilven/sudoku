(ns sudoku)

(def matrix (into [] (apply concat (repeat 9 (range 1 10)))))
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

(def sudoku [5 3 0 0 7 0 0 0 0
             6 0 0 1 9 5 0 0 0
             0 9 8 0 0 0 0 6 0
             8 0 0 0 6 0 0 0 3
             4 0 0 8 0 3 0 0 1
             7 0 0 0 2 0 0 0 6
             0 6 0 0 0 0 2 8 0
             0 0 0 4 1 9 0 0 5
             0 0 0 0 8 0 0 7 9])

(defn row
  "Return row as set for position pos"
  [mat pos]
  (let [start (* 9 (mod pos 9))]
    (-> (into #{} (subvec mat start (+ start 9)))
        (disj 0))))

(defn col
  "Return column as set for position pos."
  [mat pos]
  (let [column (mod pos 9)]
    (-> (into #{} (take-nth 9 (drop column mat)))
        (disj 0))))

(defn pos->box
  "Return box items as set for position pos."
  [mat pos]
  (let [positions (first (filter #(if (% pos) %) boxes))]
    (->> (map #(get mat %) positions)
         (remove #{0})
         (into #{}))))

(defn enumerate
  "Return enumeration vector of pos,val pairs for matrix"
  [mat]
  (reduce-kv #(conj %1 [%2 %3]) [] mat))

(defn blanks
  "Return sequence of blank positions for mat."
  [mat]
  (->> (enumerate mat)
       (filter #(= (second %) 0))))

(defn print-matrix [mat]
  (doseq [row (range 0 9)]
    (doseq [col (range 0 9)]
      (print (get mat (+ col (* row 9))) " "))
    (println)))

(comment
  (enumerate sudoku)
  (print-matrix sudoku)
  (blanks sudoku)
  (pos->box sudoku 1)
  (row sudoku 7)
  (row sudoku 2)
  (col sudoku 7)
  (col sudoku 1)
  ;;
  )
