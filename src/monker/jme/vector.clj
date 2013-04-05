(ns monker.jme.vector
  (:require [monker.util :as util]
            [clojure.core :as clj])
  (:import (com.jme3.math Vector2f Vector3f Vector4f)
           (clojure.lang PersistentVector))
  (:refer-clojure :exclude [+ - * vector]))

(defn jvector? [v] (or (instance? Vector2f)
                       (instance? Vector3f)
                       (instance? Vector4f)))

(defn ^Vector2f jvector2
  "Create a Vector2f."
  ([] Vector2f/ZERO)
  ([v] (cond
         (instance? Vector2f v) v
         (number? v) (Vector2f. v v)
         :else (util/convert-err v)))
  ([x y] (Vector2f. x y)))

(defn ^Vector3f jvector3
  "Create a Vector3f."
  ([] Vector3f/ZERO)
  ([v] (cond
         (instance? Vector3f v) v
         (number? v) (jvector3 v v v)
         :else (util/convert-err v)))
  ([x y z] (Vector3f. x y z)))

(defn ^Vector4f jvector4
  "Create a Vector4f."
  ([] Vector4f/ZERO)
  ([v] (cond
         (instance? Vector4f v) v
         (number? v) (jvector4 v v v v)
         :else (util/convert-err v)))
  ([x y z w] (Vector4f. x y z w)))

(defprotocol vector
  (add [v1 v2])
  (divide [v1 v2])
  (multiply [v1 v2])
  (negate [v])
  (subtract [v1 v2])
  (get-jme-vector [v])
  (get-v [v index])
  (set-v [v index value]))

(extend-type Vector2f
  vector
  (add [v1 v2]      (.add v1 v2))
  (divide [v1 v2]   (.divide v1 ^float v2))
  (multiply [v1 v2] (.multiply v1 (jvector2 v2)))
  (negate [v]       (.negate v))
  (subtract [v1 v2] (.subtract v1 v2))
  (get-jme-vector [v] v)
  (get-v [v index] (.get v index))
  (set-v [v index value] (.set v index value)))

(extend-type Vector3f
  vector
  (add [v1 v2]      (.add v1 v2))
  (divide [v1 v2]   (.divide v1 (jvector3 v2)))
  (multiply [v1 v2] (.multiply v1 (jvector3 v2)))
  (negate [v]       (.negate v))
  (subtract [v1 v2] (.subtract v1 v2))
  (get-jme-vector [v] v)
  (get-v [v index] (.get v index))
  (set-v [v index value] (.set v index value)))

(extend-type Vector4f
  vector
  (add [v1 v2]      (.add v1 v2))
  (divide [v1 v2]   (.divide v1 (jvector4 v2)))
  (multiply [v1 v2] (.multiply v1 (jvector4 v2)))
  (negate [v]       (.negate v))
  (subtract [v1 v2] (.subtract v1 v2))
  (get-jme-vector [v] v)
  (get-v [v index] (.get v index))
  (set-v [v index value] (.set v index value)))

(extend-type PersistentVector
  vector
  (add [v1 v2] (mapv clj/+ v1 v2))
  (divide [v1 v2] (mapv / v1 v2))
  (multiply [v1 obj]
    (cond
      (sequential? obj) (mapv clj/* v1 obj)
      (number? obj) (mapv #(clj/* % obj) v1)
      :else (util/arg-err
              "cannot multiply" v1 "with" obj)))
  (negate [v] (mapv clj/- v))
  (subtract [v1 v2] (mapv clj/- v1 v2))
  (get-jme-vector [v]
    (case (count v)
      2 (jvector2 v)
      3 (jvector3 v)
      4 (jvector4 v)
      (util/convert-err v)))
  (get-v [v index] (nth v index))
  (set-v [v index value]
    (assoc-in v [index] value)))
 
(defn +
  ([v] v)
  ([v1 v2]
   (add v1 v2))
  ([v1 v2 v3 & more]
   (apply + (+ v1 v2) v3 more)))

(defn -
  ([v] (negate v))
  ([v1 v2]
   (subtract v1 v2)))

(defn *
  ([v] v)
  ([v1 obj]
   (multiply v1 obj))
  ([v1 v2 v3 & more]
   (apply * (* v1 v2) v3 more)))

(defn div
  ([v] v)
  ([v1 obj]
   (divide v1 obj)))

(defn x [v] (get-v v 0))
(defn x! [v new-x] (set-v v 0 new-x))

(defn y [v] (get-v v 1))
(defn y! [v new-y] (set-v v 1 new-y))

(defn z [v] (get-v v 2))
(defn z! [v new-z] (set-v v 2 new-z))

(defn jvector
  "Create a Vector2f, Vector3f or Vector4f.
  
  The one argument version either takes an instance
  of the above, or a vector to convert."
  ([v]
   (cond
     (jvector? v) v
     (extends? v vector) (get-jme-vector v)
     (sequential? v) (apply jvector v)
     :else (util/convert-err v)))
  ([x y]     (jvector2 x y))
  ([x y z]   (jvector3 x y z))
  ([x y z w] (jvector4 x y z w)))
  