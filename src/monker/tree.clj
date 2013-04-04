(ns monker.tree
  (:require (monker [configure :as c]
                    [util :as util])
            [clojure.string :as cstr]))

(defn split-id-class-keyword [k]
  (let [n (name k)
        type (re-find #"^[^\.#]+" n)
        id (re-find #"(?<=#)[^\.]+" n)
        classes-str (re-find #"(?<=\.)[^#]+$" n)
        classes (if classes-str
                  (cstr/split classes-str #"\.")
                  '())]
    {:type type
     :id id
     :classes classes}))

(defn flatten-children [children]
  (reduce (fn [v n]
            (if (list? n)
              (vec (concat v n))
              (conj v n)))
          [] children))

;; =====
;; Tree
;; =====
(defn into-find-map [selector m]
  (let [{:keys [id classes]} selector]
    {:classes (into {} (map #(vector % [m]) classes))
     :ids (if id (assoc {} id m))}))

(defn merge-find-map [m1 m2]
  {:classes (merge-with concat
                        (:classes m1)
                        (:classes m2))
   :ids (merge (:ids m1)
               (:ids m2))})

(defn merge-find-maps [ms]
  (reduce merge-find-map ms))

(defn find-in-find-map [find-map selector])

(defn vec->tree [v fns]
  (let [[k & more] v
        [options & children] (if (map? (first more))
                               more (cons {} more))
        {:keys [id type classes] :as selector}
        (split-id-class-keyword k)
        children (map vec->node (flatten-children children))
        find-map (into-find-map selector (gensym))
        find-maps (cons find-map (map :find-map children))
        m {:selector selector
           :children children
           :options options
           :find-map (merge-find-maps find-maps)}]
    m))

;; =====
;; Style
;; =====
(declare merge-styles)
(defn vec->style-map [more]
  (let [{sub-styles true options false}
        (group-by vector? more)
        options (flatten-children options)
        sub-styles (map vec->style sub-styles)
        sub-style (reduce merge-styles sub-styles)]
    {:options (apply hash-map options)
     :sub-style sub-style}))

(defn vec->style [style]
  (let [[k & more] style
        style-map (vec->style-map more)
        {:keys [type id classes]} (split-id-class-keyword k)
        sm
    {:ids (if id (assoc {} id style-map))
     :classes (reduce (fn [m class]
                        (assoc m class style-map))
                      {} classes)}]
    (if type
      {:types (assoc {} type sm)}
      sm)))

(defn merge-style-map
  ([sm] sm)
  ([sm1 sm2]
   (let [{opts1 :options
          sub1 :sub-style} sm1
         {opts2 :options
          sub2 :sub-style} sm2]
     {:options (merge opts1 opts2)
      :sub-style (merge-styles sub1 sub2)})))

(defn merge-styles
  ([] nil)
  ([style] style)
  ([style1 style2]
   (let [{ids1 :ids classes1 :classes} style1
         {ids2 :ids classes2 :classes} style2]
     {:ids (merge-with merge-style-map ids1 ids2)
      :classes (merge-with merge-style-map classes1 classes2)}))
  ([style1 style2 & more]
   (let [s (merge-styles style1 style2)]
     (apply merge-styles s more))))

(defn select-style-with-type [style type]
  style)

(defn select-style-with-id [style id]
  (get-in style [:ids id]))

(defn select-style-with-class [style class]
  (get-in style [:classes class]))

(defn select-style-with-classes [style classes]
  (reduce merge-style-map
          (map #(select-style-with-class style %)
               classes)))

; (defn select-style [style selector]
;   (let [{:keys [id classes]}
;         (split-id-class-keyword selector)
;         id-style ()]
    
;     ))

(defn get-sub-style [style selector]
  (let [s (select-style style selector)]
    (merge-styles s (:sub-style s))))

; (defn style
;   ""
;   ([style]
;    (cond
;      (map? style) style
;      (vector? style) (vec->style style)))
;   ([style1 style2 & styles]
;    (let [style-maps (map style (concat [style1 style2]
;                                        styles))]
;      (reduce merge-styles style-maps))))

; (defn apply-style-map [el style-map]
;   (c/conf-int el (:options style-map)))

; (defn get-style-map [k style]
;   (let [{:keys [id classes]}
;         (split-id-class-keyword k)
;         id-map (get-in style [:ids id])
;         class-map
;         (reduce (fn [m c]
;                   (merge-style-map
;                     m
;                     (get-in style
;                             [:classes c])))
;                 {}
;                 classes)]
;     (merge class-map id-map)))
