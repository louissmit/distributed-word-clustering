
(ns applied-lt.utils
  (:require [clojure.string :refer (split)]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r])
  (:gen-class))

; Utility functions
(defn frm-save
  "Save a clojure form to file."
  [#^java.io.File file form]
  (with-open [w (java.io.FileWriter. file)]
    (binding [*out* w *print-dup* true] (prn form))))

(defn frm-load
  "Load a clojure form from file."
  [#^java.io.File file]
  (with-open [r (java.io.PushbackReader.
                  (java.io.FileReader. file))]
    (let [rec (read r)]
      rec)))

(defn show-clusters [class-assigns nr_of_classes]
  "Given the class-assigns {word : class} return clusters: [[word1 word2 ..] [word4 word5]]"
  (reduce (fn [class-vector [word class]]
            (update-in class-vector [class] conj word))
          (vec (repeat nr_of_classes []))
          class-assigns))

; Corpus preprocessing
(defn get-sents [raw_corpus start amount]
  "Given the raw corpus text, extract sentences -> [sent1 sent2 ..]"
  (take amount (drop start (pmap #(str "<s> " % " <\\s>") (split raw_corpus #"\n")))))

(defn init-class-assigns [words nr_of_classes]
  "Given unique words and nr of classes, return initial class assignment"
  (zipmap words (take (count words) (cycle (range nr_of_classes)))))

(defn get-class-counts [word_counts class-assigns nr_of_classes]
  "Given the word counts and class assignments, return words per class [c1_count c2_count ..]"
  (reduce
    (fn [res [word count]] (update-in res [(class-assigns word)] + count))
    (vec (repeat nr_of_classes 0))
    word_counts))

(defn sent-bigrams [sent]
  "Given a sentence returns its bigrams [(w1, w2) (w2, w3) ..]"
  (partition 2 1 (split sent #"\s+")))

(defn get-bigrams [fn sents]
  "Apply transformation function fn to bigrams for every sentence and count unique bigrams"
  (frequencies (mapcat #(fn (sent-bigrams %)) sents)))

(defn get-word-bigrams [sents]
  "Get all word bigram counts for sentences without transforming"
  (get-bigrams identity sents))

(defn get-C-bigrams [class-assigns sents]
  "Get all class bigrams {(c1, w2): 2 (c2, w3): 4 ..}"
  (get-bigrams (partial pmap (fn [[w1 w2]] [w1 (class-assigns w2)])) sents))

(defn get-predecessors [word-bigrams]
  "Get the predecessors of every word in vocabulary
  w_preds {w3: [w2 w8] ..}"
  (reduce (fn [w_preds [w1 w2]]
            (if (w_preds w2)
               (update-in w_preds [w2] conj w1)
               (assoc w_preds w2 #{w1})))
          {} (keys word-bigrams)))

(defn calc-LL [class_counts C-bigrams]
  "Calculate log-likelihood for the given class counts and class bigrams"
  (-
    (r/fold + (r/filter #(not (Double/isNaN %)) (r/map #(* % (Math/log %)) (vals C-bigrams))))
    (r/fold + (r/map #(* % (Math/log %)) class_counts))))