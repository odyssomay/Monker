(ns monker.ui)

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
