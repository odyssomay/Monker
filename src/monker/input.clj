(ns monker.input
  (:import (com.jme3.input.controls KeyTrigger)
           (com.jme3.input KeyInput)))

(defmacro ki [ch]
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

(defn mouse-trigger [v])

(defn joy-trigger [v])

(defn trigger
  ""
  {}
  [tr]
  (cond
    (instance? Trigger tr) tr
    (string? tr) (key-trigger tr)
    (sequential? tr)
    (let [[type v] tr]
      (case type
        :key (key-trigger v)
        :mouse (mouse-trigger v)
        :joy (joy-trigger v)
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
