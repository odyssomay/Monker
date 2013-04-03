(ns monker.ui.controls
  (:require [monker.util :as util])
  (:import de.lessvoid.nifty.controls.button.builder.ButtonBuilder
           de.lessvoid.nifty.controls.checkbox.builder.CheckboxBuilder
           de.lessvoid.nifty.controls.console.builder.ConsoleBuilder
           de.lessvoid.nifty.controls.dropdown.builder.DropDownBuilder
           de.lessvoid.nifty.controls.imageselect.builder.ImageSelectBuilder
           de.lessvoid.nifty.controls.label.builder.LabelBuilder
           de.lessvoid.nifty.controls.listbox.builder.ListBoxBuilder))

;; Standard controls
(defn configure-standard-control [^ControlBuilder this params]
  (if-let [ps (:parameters params)]
    (doseq [[k v] ps]
      (let [name (util/dash-to-camel (name k))
            value (if (or (string? v) (keyword? v))
                    (util/dash-to-camel (name v))
                    (str v))]
        (.parameter this name value))))
  (dissoc params :parameters))

(extend-type ButtonBuilder
  util/Configurable
  (configure [this params]
    (let [params (configure-standard-control this params)]
      (if-let [l (:label params)]
        (.label this l))
      (configure-element-builder
        this (dissoc params :label)))))

(extend-type CheckboxBuilder
  util/Configurable
  (configure [this params]
    (let [params (configure-standard-control this params)]
      (if-let [c? (:checked? params)]
        (.checked this c?))
      (configure-element-builder
        this (dissoc params :checked?)))))

(extend-type ConsoleBuilder
  util/Configurable
  (configure [this params]
    (let [params (configure-standard-control this params)]
      (if-let [lines (:lines params)]
        (.lines this lines))
      (configure-element-builder
        this (dissoc params :lines)))))

(extend-type DropDownBuilder
  util/Configurable
  (configure [this params]
    (let [params (configure-standard-control this params)]
      (configure-element-builder this params))))

(extend-type ImageSelectBuilder
  util/Configurable
  (configure [this params]
    (let [params (configure-standard-control this params)]
      (util/configure-helper
        params param
        :images (doseq [filename param]
                  (.addImage this filename))
        :image-height (.setImageHeight this (size-value param))
        :image-width (.setImageWidth this (size-value param)))
      (configure-element-builder
        this (dissoc params :images :image-height :image-width)))))

(extend-type LabelBuilder
  util/Configurable
  (configure [this params]
    (let [params (configure-standard-control this params)]
      (let [{:keys [label wrap?]} params]
        (if label (.label this label))
        (if wrap? (.wrap this wrap?)))
      (configure-element-builder
        this (dissoc params :label :wrap?)))))

(extend-type ListBoxBuilder
  util/Configurable
  (configure [this params]
    (let [params (configure-standard-control this params)]
      (util/configure-helper
        params param
        :display-items (.displayItems this param)
        :horizontal-scrollbar
        (case param
          :hide     (.hideHorizontalScrollbar this)
          :optional (.optionalHorizontalScrollbar this)
          :show     (.showHorizontalScrollbar this))
        :selection-mode
        (case param
          :disabled (.selectionModeDisabled this)
          ;; "Mutliple" is a typo in nifty-gui itself
          :multiple (.selectionModeMutliple this)
          :single   (.selectionModeSingle this))
        :vertical-scrollbar
        (case param
          :hide     (.hideVerticalScrollbar this)
          :optional (.optionalVerticalScrollbar this)
          :show     (.showVerticalScrollbar this)))
      (configure-element-builder this params))))
