(ns monker.test.tree
  (:use monker.tree))

(defn test1 []
  (vec->style
    [:#id1 :a 3 :b 4
     [:#id2 :b 4]
     [:#id1 :a 4]]))

(defn test2 []
  (let [s (merge-styles
            (vec->style
              [:#id1 :a 3 :b 4])
            (vec->style
              [:#id1 :a 4])
            (vec->style
              [:.class1 :bla "hello"])
            (vec->style
              [:.class2 :bla "3" :a 3]))]
    (select-style-with-classes s ["class1" "class2"])))
