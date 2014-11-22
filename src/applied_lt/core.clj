(ns applied-lt.core
  (:require [clojure.string :refer (split trim)]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [applied-lt.utils :refer :all]
            [applied-lt.pcf :refer :all]
            [applied-lt.threaded :as p])
  (:gen-class))


(defn move-to-best-class
  "Iterating function that moves words to their optimal class, returning a new assignment state."
  [[class-assigns class_counts C-bigrams word_counts word-bigrams w_preds] word]
  (if (= (class_counts (class-assigns word)) (word_counts word))
    [class-assigns class_counts C-bigrams word_counts word-bigrams w_preds]
    (let
        [current-class (class-assigns word)
         Nw (word_counts word)
         class_counts (update-in class_counts [current-class] - Nw)
         C-bigrams (edit-C-bigram word-bigrams C-bigrams w_preds - [word current-class])
         [best-class delta] (get-best-class word current-class
                                            class_counts Nw word-bigrams w_preds C-bigrams)]
      [(assoc class-assigns word best-class)
       (update-in class_counts [best-class] + Nw)
       (edit-C-bigram word-bigrams C-bigrams w_preds + [word best-class])
       word_counts word-bigrams w_preds])))

(defn repeatfn
  "Responsible for one iteration. Saves clustering to disk"
  [params]
  (let [class-assigns (nth params 0)
        class_counts (nth params 1)
        C-bigrams (nth params 2)
        word_counts (nth params 3)
        LL (calc-LL class_counts C-bigrams)]
    (println (str "Loglikelihood: " LL))
    (frm-save (io/file (str "new50kres")) [class-assigns class_counts C-bigrams LL])
    (reduce move-to-best-class params
            (keys word_counts))))


(def raw_corpus (slurp (io/resource "file.en")))

(defn -main []
  "Entry point when you run 'lein run' "
  (let
      [n 5000
       nr_of_classes 20
       iterations 10
       sents (get-sents raw_corpus 0 n)
       word_counts (frequencies (mapcat #(split % #"\s+") sents))
       class-assigns (init-class-assigns (keys word_counts) nr_of_classes)
       class_counts (get-class-counts word_counts class-assigns nr_of_classes)
       word-bigrams (get-word-bigrams sents)
       w_preds (get-predecessors word-bigrams)
       C-bigrams (get-C-bigrams class-assigns sents)]
    (println (str "Total unique words: " (count word_counts)))
    (time (doall (take iterations (iterate
                           repeatfn ; <--- change this to p/repeatfn for a threaded version !
                           [class-assigns class_counts C-bigrams word_counts word-bigrams w_preds]))))
    (println "done")))

;(-main)
;
;(def word_counts (frequencies (mapcat #(split % #"\s+") (get-sents raw_corpus 0 50000))))
;(def clusters (map (partial filter #(> (word_counts %) 50)) (show-clusters (first (frm-load (io/file "50kres"))) 200)))
;(pprint (map cons (range 200) clusters))

;(pprint (nth (frm-load (io/file "10kres")) 0))
;(clojure.string/join " " (nth clusters 11))
