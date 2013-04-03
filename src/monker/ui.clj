(ns monker.ui
  (:require (monker [util :as util])
            (monker.ui
              [element :as element]
              [style :as style]))
  (:import (de.lessvoid.nifty.tools Color SizeValue)
           de.lessvoid.nifty.Nifty
           com.jme3.niftygui.NiftyJmeDisplay
           com.jme3.app.Application))

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
;; Tools
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

(defn size-value
  "Create a SizeValue."
  {:arglists '([string]
               [size-value])}
  [obj]
  (cond
    (instance? SizeValue obj) obj
    (string? obj) (SizeValue. obj)
    :else (util/convert-err obj)))

(defn ui
  "Create a user interface."
  {:arglists '([app & options]
               [nifty-display & options])}
  [obj & {:as options}]
  (let [{:keys [style screens]} options
        nifty-display (nifty-display app)
        nifty (nifty nifty-display)]
    (doseq [screen screens]
      (let [e (element/into-element screen)]
        ()
        
    nifty-display))
