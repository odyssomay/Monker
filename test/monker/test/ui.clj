(ns monker.test.ui
  (:use monker.core
        (monker.ui element))
  (:require [monker.core :as mkr]))

(defn test1 []
  (.setLevel (java.util.logging.Logger/getLogger "")
             java.util.logging.Level/WARNING)
  (mkr/application
    :init
    (fn [app]
      (let [n (nifty-display app)
            ni (nifty n)
            s (into-element
                [:screen#start
                 [:layer
                  [:panel {:width "10%" :height "10%"
                           :background "#f00"}]
                  [:panel {:width "10%" :height "20%"
                           :background "#0f0"}]
                  [:panel {:width "10%" :height "30%"
                           :background "#00f"}]]])]
        (.addScreen ni "start" (.build s ni))
        (.gotoScreen ni "start")))
    :settings {:vsync true}))

(defn test2 []
  (.setLevel (java.util.logging.Logger/getLogger "")
             java.util.logging.Level/WARNING)
  (mkr/application
    :init
    (fn [app]
      (let [n (nifty-display app)
            ni (nifty n)
            s (into-element
                [:screen#start
                 [:layer
                  [:label {:label "hello world!"}]]])]
        (.loadStyleFile ni "nifty-default-styles.xml")
        (.loadControlFile ni "nifty-default-controls.xml")
        (.addScreen ni "start" (.build s ni))
        (.gotoScreen ni "start")))
    :settings {:vsync? true}))

(defn test3 []
  (.setLevel (java.util.logging.Logger/getLogger "")
             java.util.logging.Level/WARNING)
  (mkr/application
    :init
    (fn [app]
      (ui app
          :screens [[:screen#start
                     [:layer {:layout :center}
                      [:panel {:width "50%"
                               :height "50%"
                               :background "#f00"
                               :padding "10px"
                               :margin "10px"
                               :effects
                               {:start-screen
                                [[:border :parameters
                                  {:color "#ff0f"
                                   :border "10px"
                                   :length "infinite"}]
                                 [:fade
                                  ]]
                                }}]]]]
          :start-screen :start))
    :settings {:vsync? true}))

; (defn test-styles []
;   (style
;     [:#b-id.a-class.b-class
;      :a 3 :b 4
;      (list :x 10 :y 12)
;      [:.sub-class
;       :x :bla]]
;     [:#a-id.a-class :x 5 :y 6 :a 4]
;     [:#a-id.b-class :stuff "hello" :a 5]
;     ))
