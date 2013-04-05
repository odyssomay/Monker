(ns monker.jme.spatial
  (:import (com.jme3.scene Geometry Mesh Spatial)
           (com.jme3.math ColorRGBA Vector3f)))

(extend-type Spatial
  Configurable
  (configure [s params]
    (configure-helper 
      params param
      :translation (.setLocalTranslation s (jvector param))
      :scale (.setLocalScale s
               ^Vector3f (jvector3 param))
      :material (.setMaterial s param)
      )))

(defn mesh? [obj] (instance? Mesh obj))

(defn ^Geometry geometry
  ""
  {:arglists '([name mesh & options]
               [mesh & options])}
  [& args]
  (let [[name mesh & {:as options}]
        (if (string? (first args))
          args
          (cons (str (gensym)) args))]
    (conf-int (Geometry. name mesh)
              options)))

(defn ^Node node
  ""
  [& {:as options}]
  (Node. (or (:id options)
             (str (gensym)))))

(defn ^Spatial spatial [type & options]
  (let [f (case type
            :node node
            :geometry geometry)]
    (apply f options)))
