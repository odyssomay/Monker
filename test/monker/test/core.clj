(ns monker.test.core
  (:require [monker.core :as mkr]))

(defn application []
 (let [app (mkr/application :settings {}
                            )]))