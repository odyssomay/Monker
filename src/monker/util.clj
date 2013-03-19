(ns monker.util)

(defn arg-err [& err]
  (throw (IllegalArgumentException.
           (apply println-str err))))
