(ns monker.test.ui
  (:use [monker.ui])
  (:require [monker.core :as mkr]))

(defn test-run []
  (.setLevel (java.util.logging.Logger/getLogger "")
             java.util.logging.Level/WARNING)
  (mkr/application
    :init
    (fn [app]
      (let [n (nifty-display app)
            ni (nifty n)
            s (screen :id "start"
                      :layers
                      [(into-element
                         [:layer
                          (list [:panel {:background "#f00"
                                         :width "10%"
                                         :height "30%"}]
                                [:panel {:background "#00f"
                                         :width "10%"
                                         :height "10%"}])
                          [:panel {:background "#0f0"
                                   :width "10%"
                                   :height "20%"
                                   :halign :right}]]
                         )])]
        (.addScreen ni "start" (.build s ni))
        (.gotoScreen ni "start")))
    :settings {:frame-rate 60}
    ))

(defn test-styles []
  (style
    [:#b-id.a-class.b-class
     :a 3 :b 4
     (list :x 10 :y 12)
     [:.sub-class
      :x :bla]]
    [:#a-id.a-class :x 5 :y 6 :a 4]
    [:#a-id.b-class :stuff "hello" :a 5]
    ))
