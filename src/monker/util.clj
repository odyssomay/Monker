(ns monker.util)

(defmacro arg-err [& err]
  `(throw (IllegalArgumentException.
           ^String (apply println-str ~err))))

(defmacro req-err [option]
  `(arg-err ~option "option required!"))

(defmacro convert-err [obj]
  (arg-err "cannot be converted:" ~obj))

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
