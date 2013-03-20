(ns monker.input
  (:import (com.jme3.input.controls
             JoyAxisTrigger
             JoyButtonTrigger
             KeyTrigger
             MouseAxisTrigger
             MouseButtonTrigger)
           (com.jme3.input
             JoyInput KeyInput MouseInput)))

(defmacro ki {:private true} [ch]
  (symbol (str "KeyInput/KEY_" ch)))

(defn key-trigger [ch]
  (KeyTrigger.
    (case ch
      "A" (ki \A)
      "B" (ki \B)
      "C" (ki \C)
      "D" (ki \D)
      "E" (ki \E)
      "F" (ki \F)
      "G" (ki \G)
      "H" (ki \H)
      "I" (ki \I)
      "J" (ki \J)
      "K" (ki \K)
      "L" (ki \L)
      "M" (ki \M)
      "N" (ki \N)
      "O" (ki \O)
      "P" (ki \P)
      "Q" (ki \Q)
      "R" (ki \R)
      "S" (ki \S)
      "T" (ki \T)
      "U" (ki \U)
      "V" (ki \V)
      "W" (ki \W)
      "X" (ki \X)
      "Y" (ki \Y)
      "Z" (ki \Z)
      "0" (ki \0)
      "1" (ki \1)
      "2" (ki \2)
      "3" (ki \3)
      "4" (ki \4)
      "5" (ki \5)
      "6" (ki \6)
      "7" (ki \7)
      "8" (ki \8)
      "9" (ki \9)
      "0" (ki \0)
      "escape" (ki "ESCAPE")
      "up" (ki "UP")
      "down" (ki "DOWN")
      "left" (ki "LEFT")
      "right" (ki "RIGHT")
      )))

(defn mouse-axis-trigger [axis direction]
  (MouseAxisTrigger.
    (case axis
      :x MouseInput/AXIS_X
      :y MouseInput/AXIS_Y
      :wheel MouseInput/AXIS_WHEEL)
    (case direction
      :positive false
      :negative true)))

(defn mouse-button-trigger [button]
  (MouseButtonTrigger.
    (case button
      :left MouseInput/BUTTON_LEFT
      :right MouseInput/BUTTON_RIGHT
      :middle MouseInput/BUTTEN_MIDDLE)))

(defn mouse-trigger [type & args]
  (case type
    :axis (apply mouse-axis-trigger args)
    :button (apply mouse-button-trigger args)
    (util/arg-err
      "type must be one of: :axis :button,"
      "but got instead:" type)))

(defn trigger
  ""
  {}
  [tr]
  (cond
    (instance? Trigger tr) tr
    (string? tr) (key-trigger tr)
    (sequential? tr)
    (let [[type v :as args] tr]
      (case type
        :key (key-trigger v)
        :mouse (apply mouse-trigger (rest args))
        :joy (apply joy-trigger (rest args))
        (util/arg-err
          (str "first element must be one of: "
               ":key :mouse :joy, but got instead: "
               type))))))

(defn add-input-mappings
  ""
  {:arglists '([app mappings]
               [input-manager mappings])}
  [im mappings]
  (doseq [[k trigger] mappings]
    (.addMapping im (name k) (trigger trigger))))

(defn add-input-listeners
  ""
  {:arglists '([app listeners]
               [input-manager listeners])}
  [im listeners])
