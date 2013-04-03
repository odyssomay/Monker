(ns monker.ui.effect)

(extend-type EffectBuilder
  util/Configurable
  (configure [this params]
    (util/configure-helper
      params param
      :length (.length this param)
      :once? (.oneShot this param)
      :parameters
      (doseq [[k v] param]
        (.effectParameter this k v)))))

(defn effect [effect & {:as options}]
  (let [effect (util/dash-to-camel (name effect))]
    (util/conf-int (EffectBuilder. effect) options)))
