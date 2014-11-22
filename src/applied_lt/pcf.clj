(ns applied-lt.pcf
  (:gen-class))

; Predictive clustering functions

(defn edit-C-bigram [word-bigrams C-bigrams w_preds operator [word_a class]]
  "Update class bigram counts by either removing or adding Nwa to all (c_pred(c), word_a)"
  (reduce (fn [C-bigrams word]
            (if (C-bigrams [word class])
              (update-in C-bigrams [[word class]] operator (word-bigrams [word word_a]))
              C-bigrams))
          C-bigrams
          (w_preds word_a)))

(defn add-word [w c Nc Nw word-bigrams w_preds C-bigrams]
  "Tentatively add word w to class c and compute delta"
  (let [old (* Nc (Math/log Nc))
        Ncprime (+ Nc Nw) ; mistake in paper (you were right, Katya :)
        delta (- old (* Ncprime (Math/log Ncprime)))]
    (reduce (fn [delta v]
              (if (C-bigrams [v c]) ;only bigrams seen in the corpus
                (let [Nvc (C-bigrams [v c])
                      old (if (= Nvc 0) 0 (* Nvc (Math/log Nvc))) ;zero count bigrams did not contribute to LL
                      Nvcprime (+ Nvc (word-bigrams [v w]))
                      v_delta (- (* Nvcprime (Math/log Nvcprime)) old)]
                  (+ delta v_delta))
                delta))
            delta
            (w_preds w))))

(defn get-best-class
  [word current-class class_counts Nw word-bigrams w_preds C-bigrams]
  "Compute the best class and its associated delta for the given word"
  (reduce (fn [[best-class best-delta] c]
         (let [Nc (class_counts c)
               delta (add-word word c Nc Nw word-bigrams w_preds C-bigrams)]
           (if (> delta best-delta)
             [c delta]
             [best-class best-delta])))
          [current-class Double/NEGATIVE_INFINITY]
       (range (count class_counts))))