(ns monker.util)

(defn arg-err [err]
  (throw (IllegalArgumentException.
           err)))
