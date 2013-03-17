(ns monker.core
  (:import (com.jme3.math Vector2f Vector3f Vector4f)
           com.jme3.app.SimpleApplication
           com.jme3.system.AppSettings
           com.jme3.material.Material
           (com.jme3.scene Geometry Mesh)
           com.jme3.scene.Node
           (com.jme3.scene.shape Box Sphere Line)))

(defn no-args-error []
  (throw (IllegalArgumentException.
           "cannot be called without arguments")))

;; =====
;; Config
;; =====
(defprotocol Configurable
  (configure [this params] ""))

(defn configure-helper* [props clauses]
  (doseq [[k v] props]
    (if-let [f (get clauses k nil)]
      (f v)
      (throw (IllegalArgumentException.
               (str k " is not a valid option!"))))))

(defmacro configure-helper [params-sym param-sym & {:as clauses}]
  `(configure-helper*
     ~params-sym
     ~(into {}
            (for [[k clause] clauses]
              `[~k (fn [~param-sym] ~clause)]))))

(defn- conf-int [obj params]
  (configure obj params)
  obj)

(defn config! [obj & {:as params}] (conf-int obj params))

;; =====
;; Vector (Vector2f, Vector3f, Vector4f)
;; =====
(defn jvector? [v] (or (instance? Vector2f)
                       (instance? Vector3f)
                       (instance? Vector4f)))

(defn jvector
  "Create a Vector2f, Vector3f or Vector4f.
  
  The one argument version either takes an instance
  of the above, or a vector to convert."
  ([v]       (if (jvector? v) v (apply jvector v)))
  ([x y]     (Vector2f. x y))
  ([x y z]   (Vector3f. x y z))
  ([x y z w] (Vector4f. x y z w)))

(defn jvector2
  "Create a Vector2f."
  ([] Vector2f/ZERO)
  ([v] (Vector2f. v v))
  ([x y] (Vector2f. x y)))

(defn jvector3
  "Create a Vector3f."
  ([] Vector3f/ZERO)
  ([v] (Vector3f. v v v))
  ([x y z] (Vector3f. x y z)))

(defn jvector4
  "Create a Vector4f."
  ([] Vector4f/ZERO)
  ([v] (Vector4f. v v v v))
  ([x y z w] (Vector4f. x y z w)))

;; =====
;; Application Settings
;; =====
(defn settings? [obj] (instance? AppSettings obj))

(extend-type AppSettings
  Configurable
  (configure [s params]
    (configure-helper
      params param
      :frame-rate (.setFrameRate s param)
      :fullscreen? (.setFullscreen s param)
      :height (.setHeight s param)
      :width (.setWidth s param)
      :title (.setTitle s param)
      :load-defaults nil
      )))

(defn settings
  "Create AppSettings.
  
  Options:
   :frame-rate  Max framerate. Default: -1 (no limit).
   :fullscreen?  Default: false
   :height  Height of displayed screen in pixels.
            Default: 640.
   :width  As above. Default: 480.
   :title  Title of the window shown (only if application
           has :context-type :display - the default).
   :load-defaults  Default: true.
  "
  [& {:as args}]
  (let [{:keys [load-defaults]
         :or {load-defaults true}} args
        app-settings (conf-int (AppSettings. load-defaults)
                               args)]
    app-settings))

;; =====
;; Application
;; =====
(defn app? [obj] (instance? com.jme3.app.Application obj))

(defn param->settings [param]
  (cond
    (settings? param) param
    (map? param) (apply settings (reduce concat param))))

(extend-type SimpleApplication
  Configurable
  (configure [app params]
    (configure-helper
      params param
      :context-type nil
      :init nil
      :show-fps (.setDisplayFps app param)
      :show-statistics (.setDisplayStatView app param)
      :show-settings (.setShowSettings app param)
      :settings (.setSettings app (param->settings param))
      :update nil
      )))

(defn- jme-app-type [type]
  (case type
    :headless com.jme3.system.JmeContext$Type/Headless
    :display com.jme3.system.JmeContext$Type/Display
    :canvas com.jme3.system.JmeContext$Type/Canvas
    :offscreen com.jme3.system.JmeContext$Type/OffscreenSurface))

(defn application
  "Create a SimpleApplication
  
  The :init option is required.
  The app should only be referenced from this function,
  or the :update function.
  
  The app is started and then returned. The :init and :update
  functions run in a separate thread.
  
  jme is not thread-safe, therefore is it important that changes
  are always executed in the application's thread. This can be
  achieved either by executing the change from the :init or :update
  functions, as these already run in the correct thread. From another
  thread, monker.core/run-in-app can be used.
  
  Options:
   :context-type  One of:
                  :headless
                  :display
                  :canvas
                  :offscreen
                  
   :init  REQUIRED
          Function to run when the application
          has been started (with start!).
          Should take the app as argument.
          
   :show-fps  Default: true
   
   :show-statistics  Default: true
   
   :show-settings
   
   :update  Function to run when updating the application.
            Should take two arguments: the app and a float
  "
  [& {:as args}]
  (let [{:keys [init update context-type]
         :or {update (fn [& _])
              context-type :display}
         :as arg-map} args
        ]
    (if init
      (let [app (conf-int (proxy [SimpleApplication] []
                            (simpleInitApp [] (init this))
                            (simpleUpdate [tpf] (update this tpf))
                            )
                          args)
            ]
        (.start app (jme-app-type context-type))
        app)
      (throw (IllegalArgumentException.
               ":init argument is required")))))

(defn start!
  "Start app."
  [app] (.start app))

(defmacro run-in-app
  "Execute body on the app's thread."
  [app & body]
  `(.enqueue app (fn [] ~@body)))

;; =====
;; Asset Manager
;; =====
(defn asset-manager? [obj]
  (instance? com.jme3.asset.AssetManager obj))

(defn asset-manager
  "Takes either an
  
    com.jme3.asset.AssetManager
  or
    com.jme3.app.Application
  
  and returns an object of the first type.
  "
  {:arglists '([asset-manager]
               [app])}
  [obj]
  (if (asset-manager? obj)
    obj
    (.getAssetManager obj)))

(defn load-model
  "Returns a Spatial.
  
  asset-manager is anything that goes into
  monker.core/asset-manager."
  {:arglists '([asset-manager path & options])}
  [am-obj path & {:as options}]
  (let [am (asset-manager am-obj)]
    (conf-int (.loadModel am path)
              options)))

(defn load-audio
  "Returns AudioData.
  
  asset-manager is anything that goes into
  monker.core/asset-manager."
  {:arglists '([asset-manager path & options])}
  [am-obj path & {:as options}]
  (let [am (asset-manager am-obj)]
    (conf-int (.loadAudio am path)
              options)))

;; =====
;; Material
;; =====
(defn material? [obj] (instance? Material obj))

(extend-type Material
  Configurable
  (configure [this params])
  )

(defn material
  "Create a Material
  
  Options:
  "
  {:arglists '([app path & options]
               [asset-manager path & options])}
  [app path & {:as options}]
  (conf-int (Material. (asset-manager app) path)
            options))

;; =====
;; Color
;; =====
(defn color
  "Create a ColorRGBA"
  ([r g b] (color r g b 1.0))
  ([r g b a]
   (com.jme3.math.ColorRGBA. r g b a)))

;; =====
;; Geometry
;; =====
(extend-type Mesh
  Configurable
  (configure [mesh params]
    (configure-helper 
      params param
      :translation (.setLocalTranslation mesh (jvector param))
      :scale (.setLocalScale mesh (jvector3 param))
      )))

(defn mesh? [obj] (instance? Mesh obj))

(defn geometry
  ""
  {:arglists '([name mesh & options]
               [mesh & options])}
  [& args]
  (let [[name mesh & {:as options}]
        (if (string? (first args))
          args
          (cons (str (gensym)) args))]
    (conf-int (Geometry. name mesh)
              options)))

;; =====
;; Node
;; =====
(defn node
  ""
  ([] (Node. (str (gensym))))
  ([s] (Node. (name s))))

;; =====
;; Shape
;; =====
(defn box
  "Create a Box
  Should be treated as a mesh.
  
  The box is extended x y and z in each direction
  from the center.
  "
  [x y z] (Box. x y z))

(defn sphere
  "Create a Sphere
  Should be treated as a mesh.
  "
  [z-samples radial-samples radius]
  (Sphere. z-samples radial-samples radius))
(defn line
  "Create a Line
  Should be treated as a mesh.
  
  start and end 
  "
  ([] (Line.))
  ([start end]
   (Line. (jvector start) (jvector end))))

;; =====
;; Testing
;; =====
(defn test-it []
  (.setLevel (java.util.logging.Logger/getLogger "")
             java.util.logging.Level/WARNING)
  (application 
    :init (fn [app] (println "init!"))
    :show-fps true)
  )

;(test-it)

; (use 'clojure.pprint)

; (pprint (macroexpand-1
;           '(configure-helper 
;             params-bla param2
;             :translation (.setLocalTranslation mesh (jvector param))
;             :scale       (.setLocalScale mesh (jvector3 v))
;             )))