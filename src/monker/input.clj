(ns monker.input)

(defn into-trigger [trigger]
  )

(defn- add-input-mappings [im mappings-map]
  (doseq [[k trigger]]
    (.addMapping im (name k) (into-trigger trigger))
    ))

(defn- add-input-listeners [im listener-map])
