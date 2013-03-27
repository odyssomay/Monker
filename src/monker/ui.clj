(ns monker.ui
  (:require (monker [util :as util]))
  (:import de.lessvoid.nifty.tools.Color
           de.lessvoid.nifty.builder.ElementBuilder))

(defn nifty [app]
  (let [nifty-display (com.jme3.niftygui.NiftyJmeDisplay.
                        (.getAssetManager app)
                        (.getInputManager app)
                        (.getAudioRenderer app)
                        (.getGuiViewPort app))]
    (.addProcessor (.getGuiViewPort app) nifty-display)
    nifty-display))

(defn from-xml [nifty-display path start-screen]
  (.fromXml (.getNifty nifty-display) path start-screen))

(defn color
  ([c]
   (cond
     (instance? Color c) c
     (string? c) (Color. c)
     (number? c) (color c c c)
     (and (sequential? c)
          (or (= (count c) 1)
              (= (count c) 3)
              (= (count c) 4)))
     (apply color c)
     :else (util/arg-err
             "cannot convert to color:" c)))
  ([r g b] (color r g b 1.0))
  ([r g b a] (Color. r g b a))
  )

(defn- margin! [el m]
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

(defn- padding! [el p]
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

(extend-type ElementBuilder
  util/Configurable
  (configure [this params]
    (util/configure-helper
      params param
      :background (.backgroundColor this param)
      :background-image (.backgroundImage this param)
      :layout (case param
                :absolute (.childLayoutAbsolute this)
                :absolute-inside (.childLayoutAbsoluteInside this)
                :center (.childLayoutCenter this)
                :horizontal (.childLayoutHorizontal this)
                :overlay (.childLayoutOverlay this)
                :vertical (.childLayoutVertical this)
                )
      :color (.color this (color param))
      :control (.control this param)
      :controller (.controller this param)
      :focusable? (.focusable this (boolean param))
      :font (.font this param)
      :height (.height this (str param))
      :id (.id this (name param))
      :inset (.inset this (str param))
      :margin (margin! this param)
      :name (.name this param)
      :padding (padding! this param)
      :selection-color (.selectionColor this param)
      :style (.style this param)
      :text (.text this param)
      :visible? (.visible this (boolean param))
      :width (.width this (str param))
      :x (.x this (str param))
      :y (.y this (str param))
      )))
