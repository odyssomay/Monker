(ns monker.util
  (:require [clojure.string :as cstr]))

(defmacro arg-err [& err]
  `(throw (IllegalArgumentException.
           ^String (apply println-str (vector ~@err)))))

(defmacro req-err [option]
  `(arg-err ~option "option required!"))

(defmacro convert-err [obj]
  `(arg-err "cannot be converted:" ~obj))

;; =====

(defn dash-to-camel [string]
  (let [string (name string)
        words (.split ^String string "-")
        capitalized (map cstr/capitalize (rest words))]
    (apply str (first words) capitalized)))
