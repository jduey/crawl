(ns crawl.monad
  (:require [clojure.algo.monads :refer [state-t maybe-m with-monad
                                         domonad m-seq m-plus]]))

(defn update-state [f & args]
  (fn [state]
    [state (apply f state args)]))

(defn update-val [k f & args]
  (fn [state]
    [(get state k)
     (assoc state k (apply f (get state k) args))]))

(def crawl-m (state-t maybe-m))

(defmacro with-state [& operations]
  (if (= 1 (count operations))
    `(with-monad crawl-m
                 ~(first operations))
    `(with-monad crawl-m
                 (m-seq (list ~@operations)))))

(defmacro do-state [& args]
  `(domonad crawl-m ~@args))

(with-monad crawl-m
  (defn optional [parser]
    (m-plus parser (m-result nil))) 

  (def one-or-more) 

  (defn none-or-more [parser]
    (optional (one-or-more parser))) 

  (defn one-or-more [parser]
    (do-state
      [a parser
       as (none-or-more parser)]
      (cons a as)))) 

