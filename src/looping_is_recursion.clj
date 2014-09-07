(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n) acc
                     (recur (* base acc) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq)) (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
     (let [helper (fn [a b]
                    (cond (and (empty? a) (empty? b)) true
                          (and (empty? a) (seq b)) false
                          (and (seq a) (empty? b)) false
                          (not= (first a) (first b)) false
                          :else
                          (recur (rest a) (rest b))))]
       (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [n 0
         coll a-seq]
    (cond (empty? coll) nil
          (pred (first coll)) n
          :else (recur (inc n) (rest coll)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         coll a-seq]
    (cond (and (empty? coll) (= n 0)) nil
          (and (empty? coll)) (/ sum n)
          :else (recur (inc n) (+ sum (first coll)) (rest coll)))))

(defn parity [a-seq]
  (loop [odd-set #{}
         coll a-seq]
    (cond (empty? coll) odd-set
          :else (recur (if (contains? odd-set (first coll))
                         (disj odd-set (first coll))
                         (conj odd-set (first coll)))
                       (rest coll)))))

(defn fast-fibo [n]
  (loop [f0 0
         f1 1
         num 2
         ]
    (cond (< n 2) n
          (= n num) (+ f0 f1)
          :else (recur f1 (+ f0 f1) (inc num)))))

(defn cut-at-repetition [a-seq]
  (loop [seen-set #{}
         cut-vec []
         coll a-seq]
    (cond (empty? coll) cut-vec
          (contains? seen-set (first coll)) cut-vec
          :else (recur (conj seen-set (first coll))
                       (conj cut-vec (first coll))
                       (rest coll)))))
