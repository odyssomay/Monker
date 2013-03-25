(ns monker.ui
  (:require (monker [util :as util])))

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
