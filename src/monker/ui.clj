(ns monker.ui
  (:require (monker [util :as util])
            (monker.ui
              [element :as element]
              [style :as style]))
  (:import (de.lessvoid.nifty.builder
             ScreenBuilder)
           de.lessvoid.nifty.Nifty
           com.jme3.niftygui.NiftyJmeDisplay
           com.jme3.app.Application))

;; =====
;; Nifty
;; =====
(defn ^NiftyJmeDisplay nifty-display
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

(defn ^Nifty nifty
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

(defn ui
  "Create a user interface."
  {:arglists '([app & options]
               [nifty-display & options])}
  [obj & {:as options}]
  (let [{:keys [style screens start-screen]} options
        nifty-display (nifty-display obj)
        nifty (nifty nifty-display)]
    (doseq [screen screens]
      (let [e ^ScreenBuilder (element/into-element screen)
            built (.build e nifty)]
        (.addScreen nifty (.getScreenId built) built)))
    (.gotoScreen nifty (or (name start-screen) "start"))
    nifty-display))
