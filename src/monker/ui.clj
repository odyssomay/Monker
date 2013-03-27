(ns monker.ui
  (:require (monker [util :as util]))
  (:import de.lessvoid.nifty.tools.Color
           de.lessvoid.nifty.Nifty
           com.jme3.niftygui.NiftyJmeDisplay
           com.jme3.app.Application
           (de.lessvoid.nifty.builder
             ElementBuilder
             ImageBuilder
             LayerBuilder
             PanelBuilder
             PopupBuilder
             ScreenBuilder
             TextBuilder)))

;; =====
;; Nifty
;; =====
(defn nifty-display
  "Create a NiftyJmeDisplay
  and attach it to the application.
  "
  {:arglists '([app])}
  [obj]
  (cond
    (instance? NiftyJmeDisplay obj) obj
    (instance? Application obj)
    (let [app ^Application obj
          nifty-display (NiftyJmeDisplay.
                          (.getAssetManager app)
                          (.getInputManager app)
                          (.getAudioRenderer app)
                          (.getGuiViewPort app))]
      (.addProcessor (.getGuiViewPort app) nifty-display)
      nifty-display)
    :else (util/convert-err obj)))

(defn nifty
  "Get a Nifty object."
  {:arglists '([nifty] [nifty-display])}
  [obj]
  (cond
    (instance? NiftyJmeDisplay obj)
    (.getNifty ^NiftyJmeDisplay obj)
    (instance? Nifty obj) obj
    :else (util/convert-err obj)))

(defn from-xml
  "Load ui from xml."
  [nifty-display path start-screen]
  (.fromXml ^Nifty (nifty nifty-display)
            path start-screen))

;; =====
;; Style
;; =====
(defn split-id-class-keyword [k]
  (let [n (name k)
        type (re-find #"^[^\.#]+" n)
        id (re-find #"(?<=#)[^\.]+" n)
        classes (seq (.split ^String
                             (re-find #"(?<=\.)[^#]+$" n)
                             "\\."))]
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

;; =====
;; Elements
;; =====
(defn color
  "Create a Color.
  (de.lessvoid.nifty.tools.Color)
  
  The one argument version can be called with:
  
   a Color, which is returned.
  
   a string containing a html-style (hexadecimal) color.
   
   a number, specifies a gray color.
             0 <= c <= 1
   
   a list/vector, this function is applied to it.
  
  Examples:
   (color (Color. 0 0 0))
   => (Color. 0 0 0)
   
   (color \"#fafafa\")
   => (Color. 0.98039216 0.98039216 0.98039216,1.0)
   
   (color 0.2)
   => (Color. 0.2 0.2 0.2)
   
   (color [0.1 0.4 1.0])
   => (color 0.1 0.4 1.0)
   => (Color. 0.1 0.4 1.0)
  "
  ([c]
   (cond
     (instance? Color c) c
     (string? c) (Color. ^String c)
     (number? c) (color c c c)
     (and (sequential? c)
          (or (= (count c) 1)
              (= (count c) 3)
              (= (count c) 4)))
     (apply color c)
     :else (util/arg-err
             "cannot convert to color:" c)))
  ([r g b] (color r g b 1.0))
  ([r g b a] (Color. r g b a)))

(defn margin! [^ElementBuilder el m]
  (cond
    (number? m) (.margin el (str m))
    (and (sequential? m)
         (= (count m) 4))
    (let [[top right bottom left] m]
      (doto el
        (.marginTop top)
        (.marginRight right)
        (.marginBottom bottom)
        (.marginLeft left)))))

(defn padding! [^ElementBuilder el p]
  (cond
    (number? p) (.margin el (str p))
    (and (sequential? p)
         (= (count p) 4))
    (let [[top right bottom left] p]
      (doto el
        (.paddingTop top)
        (.paddingRight right)
        (.paddingBottom bottom)
        (.paddingLeft left)))))

(defn add-item [^ElementBuilder el item]
  (condp instance? item
    ImageBuilder (.image el item)
    PanelBuilder (.panel el item)
    TextBuilder  (.text el ^TextBuilder item)))

(defn add-items [el items]
  (println "adding items")
  (doseq [item items]
    (add-item el item)))

(defn configure-element-builder
  [^ElementBuilder this params]
  (util/configure-helper
    params param
    :background (.backgroundColor
                  this ^Color (color param))
    :background-image (.backgroundImage this param)
    :color (.color this ^Color (color param))
    :controller (.controller this param)
    :focusable? (.focusable this (boolean param))
    :font (.font this param)
    :halign (case param
              :center (.alignCenter this)
              :left (.alignLeft this)
              :right (.alignRight this))
    :height (.height this (str param))
    :id (.id this (name param))
    :inset (.inset this (str param))
    :items (add-items this param)
    :layout (case param
              :absolute (.childLayoutAbsolute this)
              :absolute-inside (.childLayoutAbsoluteInside this)
              :center (.childLayoutCenter this)
              :horizontal (.childLayoutHorizontal this)
              :overlay (.childLayoutOverlay this)
              :vertical (.childLayoutVertical this))
    :margin (margin! this param)
    :name (.name this param)
    :padding (padding! this param)
    :selection-color (.selectionColor
                       this ^Color (color param))
    :style (.style this param)
    :valign (case param
              :center (.valignCenter this)
              :bottom (.valignBottom this)
              :top (.valignBottom this))
    :visible? (.visible this (boolean param))
    :width (.width this (str param))
    :x (.x this (str param))
    :y (.y this (str param))
    ))

(extend-type ElementBuilder
  util/Configurable
  (configure [this params]
    (configure-element-builder this params)))

(extend-type TextBuilder
  util/Configurable
  (configure [this params]
    (configure-element-builder
      this (dissoc params :wrap?))
    (.wrap this (boolean (:wrap? params)))))

(defn element
  "Create an element for use in a screen.
  
  Types:
   :image
   :layer
   :panel
   :popup
   :text
   
  Options:
   :align
   :background
   :background-image
   :layout
   :color
   :controller
   :focusable?
   :font
   :height
   :id
   :inset
   :items
   :margin
   :name
   :padding
   :selection-color
   :style
   :valign
   :visible?
   :width
   :x
   :y
  "
  {:arglists '([type & options])}
  [type & {:as options}]
  (let [options (merge {:layout :horizontal}
                       options)
        builder
        (case type
          :image (ImageBuilder.)
          :layer (LayerBuilder.)
          :panel (PanelBuilder.)
          :popup (PopupBuilder.)
          :text  (TextBuilder.))]
    (util/conf-int builder options)))

(declare into-element)
(defn vec->element
  ""
  {:arglists '([v]
               [v style])}
  ([v] (vec->element v nil))
  ([v s]
   (let [[type & more] v
         [options & children]
         (if (map? (first more))
           more
           (cons {} more))
         options (merge {:items (map into-element
                                     children)}
                        options)]
     (apply element type (reduce concat options)))))

(defn into-element
  ""
  {:arglists '([element]
               [element style])}
  ([el] (into-element el nil))
  ([el s]
   (cond
     (instance? ElementBuilder el) el
     (vector? el) (vec->element el)
     :else (util/convert-err el))))

(extend-type ScreenBuilder
  util/Configurable
  (configure [this params]
    (util/configure-helper
      params param
      :controller (.controller this param)
      :focus (.defaultFocusElement this param)
      :layers (doseq [layer param]
                (.layer this layer)))))

(defn screen [& {:as options}]
  (let [{:keys [id]} options]
    (if-not id (util/req-err :id))
    (util/conf-int (ScreenBuilder. id)
                   (dissoc options :id))))
