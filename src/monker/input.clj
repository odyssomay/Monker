(ns monker.input
  (:require (monker [util :as util]))
  (:import (com.jme3.input.controls
             JoyAxisTrigger
             JoyButtonTrigger
             KeyTrigger
             MouseAxisTrigger
             MouseButtonTrigger
             Trigger)
           (com.jme3.input
             JoyInput KeyInput MouseInput)))

(defmacro ki {:private true} [ch]
  (symbol (str "KeyInput/KEY_" ch)))

(defn key-trigger
  "Create a KeyTrigger.
  
  ch is a string denoting the key trigger
  to create, which can be any of:
   A-Z
   0-9
   escape
   up
   down
   left
   right
  "
  [ch]
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
      :middle MouseInput/BUTTON_MIDDLE)))

(defn mouse-trigger
  "Create either:
   MouseAxisTrigger
   MouseButtonTrigger.
  
  The first argument denotes the type,
  and can be any of:
   :axis for MouseAxisTrigger.
         Then axis is :x, :y or :wheel
         and direction is :positive
         or :negative.
         
   :button for MouseButtonTrigger.
           Then button is one of
           :left :right :middle.
  "
  {:arglists '([:axis axis direction]
               [:button button])}
  [type & args]
  (case type
    :axis (apply mouse-axis-trigger args)
    :button (apply mouse-button-trigger args)
    (util/arg-err
      "type must be one of: :axis :button,"
      "but got instead:" type)))

(defn trigger
  "Create a Trigger.
  
  If argument is a Trigger, it is returned.
  
  If argument is an Input (e.g. KeyInput, MouseInput),
  A trigger is created from the input.
  
  If the argument is a string (key), a key-trigger
  is created. See monker.input/key-trigger for details.
  
  If a sequence is provided, the first element designates
  the type. Type can be any of: :key :mouse. The args
  are then applied to either monker.input/key-trigger
  or monker.input/mouse-trigger, respectively.
  "
  {:arglists '([trigger]
               [input]
               [key]
               [[type & args]])}
  [tr]
  (cond
    (instance? Trigger tr) tr
    (string? tr) (key-trigger tr)
    (sequential? tr)
    (let [[type v :as args] tr]
      (case type
        :key (key-trigger v)
        :mouse (apply mouse-trigger (rest args))
        (util/arg-err
          (str "first element must be one of: "
               ":key :mouse, but got instead: "
               type))))))

(defn add-input-mappings
  "Add input mappings to the input manager
  (or the input manager of the app).
  
  mappings is a map where the keys denote
  which mapping should be triggered. It should
  match the keys provided to
  monker.input/add-input-listeners.
  
  The values should be a list of objects that
  can be converted to a Trigger by monker.input/trigger.
  
  Example mappings:
  {:forward [\"up\" \"w\"]
   :backward [\"down\" \"s\"]
   :attack [\"space\"]
   :zoom-out [[:mouse :axis :wheel :positive]]
   :zoom-in [[:mouse :axis :wheel :negative]]
   }
  
  See monker.input/add-input-listeners for
  the corresponding listeners.
  "
  {:arglists '([app mappings]
               [input-manager mappings])}
  [im mappings]
  (doseq [[k trigger] mappings]
    (.addMapping im (name k) (trigger trigger))))

(defn add-input-listeners
  "Add listeners to the input manager
  (or the input manager of the app).
  
  listeners is a map where the keys denote
  the mapping, provided to
  monker.input/add-input-mappings
  
  Example listeners:
  {:forward ...
   :backward ...
   :attack ...
   :zoom-out ...
   :zoom-in ...
   }
  "
  {:arglists '([app listeners]
               [input-manager listeners])}
  [im listeners]
  )
