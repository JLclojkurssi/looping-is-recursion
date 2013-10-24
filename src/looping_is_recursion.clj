(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [subseq]
                 (if (empty? (rest subseq))
                 (first subseq)
                 (recur (rest subseq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (if (not (== (count seq1) (count seq2)))
    false
    (loop [aseq seq1
           bseq seq2]
      (cond
       (not (= (first aseq) (first bseq))) false
       (and (empty? aseq) (empty? bseq)) true
       :else (recur (rest aseq) (rest bseq))))))

(defn find-first-index [pred a-seq]
  (loop [seq1 a-seq
         n 0]
    (cond
     (empty? seq1) nil
     (pred (first seq1)) n
     :else (recur (rest seq1) (inc n)))))

(defn avg [a-seq]
  (loop [sum 0
         downto (count a-seq)]
    (if (zero? downto)
      (/ sum (count a-seq))
      (recur (+ sum (get a-seq (dec downto))) (dec downto)))))


(defn parity [a-seq]
  (loop [seq1 a-seq
         ress #{}]
    (cond
     (empty? seq1) ress
     (contains? ress (first seq1)) (recur
                                    (rest seq1)
                                    (disj (set ress) (first seq1)))
     :else (recur (rest seq1) (conj (set ress) (first seq1))))))

(defn fast-fibo [n]
  (loop [x 1
         cur 0
         prev 0]
    (cond
     (> x n) cur
     (== x 1) (recur (inc x) 1 0)
     :else (recur (inc x) (+ cur prev) cur))))

(defn cut-at-repetition [a-seq]
  (loop [cseq []
         n 0]
    (if (or (== (count a-seq) n) (some #{(get a-seq n)} cseq))
        cseq
        (recur (conj cseq (get a-seq n)) (inc n)))))
