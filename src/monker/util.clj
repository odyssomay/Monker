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
;; Config
;; =====
(defprotocol Configurable
  (configure [this params] ""))

(defn configure-helper* [props clauses]
  (doseq [[k v] props]
    (if-let [f (get clauses k nil)]
      (f v)
      (throw (IllegalArgumentException.
               (str k " is not a valid option!"))))))

(defmacro configure-helper [params-sym param-sym & {:as clauses}]
  `(configure-helper*
     ~params-sym
     ~(into {}
            (for [[k clause] clauses]
              `[~k (fn [~param-sym] ~clause)]))))

(defn conf-int [obj params]
  (configure obj params)
  obj)

;; =====

(defn dash-to-camel [string]
  (let [words (.split ^String string "-")
        capitalized (map cstr/capitalize (rest words))]
    (apply str (first words) capitalized)))
