(ns monker.ui.element
  (:require monker.ui.controls)
  (:import (de.lessvoid.nifty.builder
             ControlBuilder
             ElementBuilder
             ImageBuilder
             LayerBuilder
             PanelBuilder
             PopupBuilder
             ScreenBuilder
             TextBuilder)
           de.lessvoid.nifty.controls.button.builder.ButtonBuilder
           de.lessvoid.nifty.controls.checkbox.builder.CheckboxBuilder
           de.lessvoid.nifty.controls.console.builder.ConsoleBuilder
           de.lessvoid.nifty.controls.dropdown.builder.DropDownBuilder
           de.lessvoid.nifty.controls.imageselect.builder.ImageSelectBuilder
           de.lessvoid.nifty.controls.label.builder.LabelBuilder
           de.lessvoid.nifty.controls.listbox.builder.ListBoxBuilder))

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
    TextBuilder  (.text el ^TextBuilder item)
    ControlBuilder (.control el item)))

(defn add-items [el items]
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
    :controller (.controller this ^Controller param)
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

(extend-type ScreenBuilder
  util/Configurable
  (configure [this params]
    (util/configure-helper
      params param
      :controller (.controller this param)
      :focus (.defaultFocusElement this param)
      :id nil
      :items (doseq [layer param]
                (.layer this layer)))))

(defn element
  "Create an element.
  
  Types:
   :image
   :layer
   :panel
   :popup
   :screen *
   :text
  
  Types (controls):
   :button *
   :checkbox
   :console *
   :drop-down *
   :image-select *
   :label
   :list-box
  
  * Requires the :id option.
  
  
  Options (except for :screen - see below):
   :background - a color
   :background-image
   :layout :absolute :absolute-inside :center
           :horizontal :overlay or :vertical
   :color
   :controller
   :focusable?
   :font  a string pointing to a font on
          the classpath.
   :halign  :center :left or :right
   :height
   :id
   :inset
   :items - a sequence of elements to
            add as children to this element.
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
  
  Options, for :screen type:
   :controller
   
   :focus  id of the element to focus when
           the screen is shown.
   
   :items  A list/vector of layers to show in this screen.
           Layers are ordered back to front. That is,
           the first element in the list is in the back.
           The second element is in front of the first.
           The third element is in front of the second.
           And so on.
  "
  {:arglists '([type & options])}
  [type & {:as options}]
  (let [get-id (fn []
                 (or (:id options)
                     (util/arg-err
                       ":id option required for element type "
                       type)))
        options (if-not (= type :screen)
                  (merge {:layout :horizontal}
                         options)
                  options)
        builder
        (case type
          :image  (ImageBuilder.)
          :layer  (LayerBuilder.)
          :panel  (PanelBuilder.)
          :popup  (PopupBuilder.)
          :screen (ScreenBuilder. (get-id))
          :text   (TextBuilder.)
          ;; controls
          :button (ButtonBuilder. (get-id))
          :checkbox (CheckboxBuilder.)
          :console (ConsoleBuilder. (get-id))
          :drop-down (DropDownBuilder. (get-id))
          :image-select (ImageSelectBuilder. (get-id))
          :label (LabelBuilder.)
          :list-box (ListBoxBuilder.))]
    (util/conf-int builder options)))

(declare into-element)
(defn vec->options [v]
  (let [[options & children]
        (if (map? (first v))
          v (cons {} v))
        children (reduce (fn [v n]
                           (if (list? n)
                             (vec (concat v n))
                             (conj v n)))
                         [] children)
        options (merge {:items children}
                       options)]
    options))

(defn vec->element
  {:arglists '([v]
               [v style])}
  ([v] (vec->element v nil))
  ([v s]
   (let [{:keys [id type]} (split-id-class-keyword (first v))
         options (vec->options (rest v))
         options (assoc options
                   :items
                   (map #(into-element % s)
                        (:items options)))
         options (if id
                   (assoc options :id id)
                   options)
         options (reduce concat options)]
     (apply element (keyword type) options))))

(defn into-element
  "Convert element into an element.
  
  Element can either be:
   Of type ElementBuilder. This is what you get
   from calling monker.ui/element.
   
   A vector.
  "
  {:arglists '([element]
               [element style])}
  ([el] (into-element el nil))
  ([el s]
   (cond
     (instance? ElementBuilder el) el
     (vector? el) (vec->element el)
     :else (util/convert-err el))))
