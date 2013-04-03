(ns monker.ui.style
  (:require [clojure.string :as cstr]))

(defn split-id-class-keyword [k]
  (let [n (name k)
        type (re-find #"^[^\.#]+" n)
        id (re-find #"(?<=#)[^\.]+" n)
        classes (cstr/split (or (re-find #"(?<=\.)[^#]+$" n)
                                "")
                            #"\.")]
    {:type type
     :id id
     :classes classes}))

(declare style)
(defn vec->style-map [more]
  (let [{sub-styles true options false}
        (group-by vector? more)
        options (reduce (fn [v n]
                          (if (list? n)
                            (vec (concat v n))
                            (conj v n)))
                        [] options)
        sub-style (cond
                    (>= (count sub-styles) 2)
                    (reduce style sub-styles)
                    (>= (count sub-styles) 1)
                    (apply style sub-styles)
                    :else nil)]
    {:options (apply hash-map options)
     :sub-style sub-style
     }))

(defn vec->style [style]
  (let [[k & more] style
        style-map (vec->style-map more)
        {:keys [type id classes]} (split-id-class-keyword k)]
    {:ids (if id (assoc {} id style-map))
     :classes (reduce (fn [m class]
                        (assoc m class style-map))
                      {} classes)}))

(declare merge-styles)
(defn merge-style-map [sm1 sm2]
  (let [{opts1 :options
         sub1 :sub-style} sm1
        {opts2 :options
         sub2 :sub-style} sm2]
    {:options (merge opts1 opts2)
     :sub-style (merge-styles sub1 sub2)}))

(defn merge-styles [style1 style2]
  (let [{ids1 :ids classes1 :classes} style1
        {ids2 :ids classes2 :classes} style2]
    {:ids (merge-with merge-style-map ids1 ids2)
     :classes (merge-with merge-style-map classes1 classes2)}))

(defn style
  ""
  ([style]
   (cond
     (map? style) style
     (vector? style) (vec->style style)))
  ([style1 style2 & styles]
   (let [style-maps (map style (concat [style1 style2]
                                       styles))]
     (reduce merge-styles style-maps))))

(defn apply-style-map [el style-map]
  (util/conf-int el (:options style-map)))

(defn get-style-map [k style]
  (let [{:keys [id classes]}
        (split-id-class-keyword k)
        id-map (get-in style [:ids id])
        class-map
        (reduce (fn [m c]
                  (merge-style-map
                    m
                    (get-in style
                            [:classes c])))
                {}
                classes)]
    (merge class-map id-map)))
