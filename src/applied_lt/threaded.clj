(ns applied-lt.threaded
  (:require [applied-lt.pcf :refer :all]
            [applied-lt.utils :refer (calc-LL frm-save)]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r])
  (:gen-class))

(defn move-to-best-class
  [[class-assigns class_counts C-bigrams word_counts word-bigrams w_preds] word best-class]
  "Iterating function that moves words to their optimal class, returning a new assignment state."
  (if (= (class_counts (class-assigns word)) (word_counts word))
    [class-assigns class_counts C-bigrams word_counts word-bigrams w_preds]
    (let [current-class (class-assigns word)
          Nw (word_counts word)
          class_counts (update-in class_counts [current-class] - Nw)
          C-bigrams (edit-C-bigram word-bigrams C-bigrams w_preds - [word current-class])]
      [(assoc class-assigns word best-class)
       (update-in class_counts [best-class] + Nw)
       (edit-C-bigram word-bigrams C-bigrams w_preds + [word best-class])
       word_counts word-bigrams w_preds])))

(defn reduce-partition [class-assigns class_counts word-bigrams w_preds C-bigrams]
  "Returns a reducing function that calculates new assignments for a partition of the data"
  (fn
    ([] {})
    ([new-assigns word Nw]
     (if (or (> (rand) (/ 1 3));only evaluate 1/3 of the partition (see paper)
             (= (class_counts (class-assigns word)) Nw));classcount cant be zero
       new-assigns
       (let [current-class (class-assigns word)
             new-cc (update-in class_counts [current-class] - Nw)
             new-cb (edit-C-bigram word-bigrams C-bigrams w_preds - [word current-class])
             [best-class delta] (get-best-class word current-class new-cc Nw word-bigrams w_preds new-cb)]
         (if (not (= best-class current-class))
           (assoc new-assigns word best-class)
           new-assigns))))))

(defn combine-partitions
  "Merges all new class assignments"
  ([] {})
  ([& new-assigns]
   (apply merge new-assigns)))

(defn repeatfn
  [[class-assigns class_counts C-bigrams word_counts word-bigrams w_preds]]
  "Responsible for one iteration. Saves clustering to disk"
  (let [LL (calc-LL class_counts C-bigrams)
        rp-func (reduce-partition class-assigns class_counts word-bigrams w_preds C-bigrams)
        new-assigns (r/fold combine-partitions rp-func word_counts)]
    (println "-------------")
    (println (str "Loglikelihood: " LL))
    (println (str "Exchanged words: " (count new-assigns) " " (* 100.0 (/ (count new-assigns) (count word_counts))) "%"))
    (frm-save (io/file "p50kres") [class-assigns class_counts C-bigrams LL])
    (reduce-kv move-to-best-class
               [class-assigns class_counts C-bigrams word_counts word-bigrams w_preds]
               new-assigns)))