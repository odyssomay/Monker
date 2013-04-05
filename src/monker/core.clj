(ns monker.core
  (:use [monker.configure
         :only [Configurable
                conf-int configure-helper]])
  (:require (monker [configure :as c]
                    [input :as input]
                    [util :as util]))
  (:import com.jme3.asset.AssetManager
           (com.jme3.math ColorRGBA Vector2f Vector3f Vector4f)
           (com.jme3.app
             Application
             SimpleApplication)
           com.jme3.system.AppSettings
           com.jme3.material.Material
           (com.jme3.scene Geometry Mesh Spatial)
           com.jme3.scene.Node
           (com.jme3.scene.shape Box Sphere Line)))

(defmacro import-symbols [ns-symbol symbols]
  `(do ~@(for [s symbols]
           (let [qualified (symbol (str ns-symbol "/" s))
                 doc (str "Mirrored from " qualified ".")]
             `(def ~s ~doc ~qualified)))))

(import-symbols
  monker.configure
  [config!])

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

(defn ^Vector2f jvector2
  "Create a Vector2f."
  ([] Vector2f/ZERO)
  ([v] (Vector2f. v v))
  ([x y] (Vector2f. x y)))

(defn ^Vector3f jvector3
  "Create a Vector3f."
  ([] Vector3f/ZERO)
  ([v] (Vector3f. v v v))
  ([x y z] (Vector3f. x y z)))

(defn ^Vector4f jvector4
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
      :load-defaults nil
      :title (.setTitle s param)
      :vsync? (.setVSync s param)
      :width (.setWidth s param))))

(defn ^AppSettings settings
  "Create AppSettings.
  
  Options:
   :frame-rate  Max framerate. Default: -1 (no limit).
   :fullscreen?  Default: false
   :height  Height of displayed screen in pixels.
            Default: 640.
   :load-defaults  Default: true.
   :title  Title of the window shown (only if application
           has :context-type :display - the default).
   :vsync?  Default: false.
   :width  Default: 480.
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
(defn app? [obj] (instance? Application obj))

(defn param->settings [param]
  (cond
    (settings? param) param
    (map? param) (apply settings (reduce concat param))))

(extend-type SimpleApplication
  Configurable
  (configure [app params]
    (configure-helper
      params param
      :show-fps (.setDisplayFps app param)
      :show-statistics (.setDisplayStatView app param)
      :show-settings (.setShowSettings app param)
      :settings (.setSettings app (param->settings param)))))

(defn- jme-app-type [type]
  (case type
    :headless com.jme3.system.JmeContext$Type/Headless
    :display com.jme3.system.JmeContext$Type/Display
    :canvas com.jme3.system.JmeContext$Type/Canvas
    :offscreen com.jme3.system.JmeContext$Type/OffscreenSurface))

(defn ^SimpleApplication application
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
    
   :settings  Either an AppSettings object, or a map
              that is converted using monker.core/settings.
   
   :show-fps  Default: true
   
   :show-statistics  Default: true
   
   :show-settings
   
   :stop  Function to run when the application is stopping.
          Should take the app as argument.
   
   :update  Function to run when updating the application.
            Should take two arguments: the app and a float.
  "
  [& {:as args}]
  (let [{:keys [init update stop context-type]
         :or {update (fn [& _])
              stop (fn [& _])
              context-type :display}
         :as arg-map} args
        ]
    (if init
      (let [args (dissoc args :context-type :init :stop :update)
            ^SimpleApplication
            app
            (conf-int (proxy [SimpleApplication] []
                        (simpleInitApp [] (init this))
                        (simpleUpdate [tpf] (update this tpf))
                        (destroy []
                          (let [^SimpleApplication this this]
                            (stop this)
                            (proxy-super destroy))))
                      args)]
        (.start app (jme-app-type context-type))
        app)
      (util/req-err :init))))

(defn start!
  "Start app."
  [app] (.start ^Application app))

(defn stop!
  "Stop the application
  
  (stop! app) is the same as
  (stop! app false).
  
  If wait? is truthy, stop! blocks
  until the application is fully stopped.
  Otherwise, stop! returns instantly.
  "
  ([app] (.stop ^Application app))
  ([app wait?] (.stop ^Application app (boolean wait?))))

(defmacro run-in-app
  "Execute body on the app's thread."
  [app & body]
  `(.enqueue app (fn [] ~@body)))

;; =====
;; Asset Manager
;; =====
(defn asset-manager? [obj]
  (instance? com.jme3.asset.AssetManager obj))

(defn ^AssetManager asset-manager
  "Takes either an
  
    com.jme3.asset.AssetManager
  or
    com.jme3.app.Application
  
  and returns an object of the first type.
  "
  {:arglists '([asset-manager]
               [app])}
  [obj]
  (cond
    (asset-manager? obj) obj
    (app?) (.getAssetManager ^Application obj)
    :else (util/convert-err obj)))

(defn load-model
  "Returns a Spatial.
  
  asset-manager is anything that goes into
  monker.core/asset-manager."
  {:arglists '([asset-manager path & options])}
  [am-obj path & {:as options}]
  (let [am (asset-manager am-obj)]
    (conf-int (.loadModel am ^String path)
              options)))

(defn load-audio
  "Returns AudioData.
  
  asset-manager is anything that goes into
  monker.core/asset-manager."
  {:arglists '([asset-manager path & options])}
  [am-obj path & {:as options}]
  (let [am (asset-manager am-obj)]
    (conf-int (.loadAudio am ^String path)
              options)))

;; =====
;; Input manager
;; =====
(extend-type com.jme3.input.InputManager
  Configurable
  (configure [this params]
    (configure-helper
      params param
      :mappings (input/add-input-mappings this param)
      :action-listeners (input/add-action-listeners this param)
      :analog-listeners (input/add-analog-listeners this param)
      )))

(def input-manager input/input-manager)

;; =====
;; Color
;; =====
(defn ^ColorRGBA color
  "Create a ColorRGBA"
  ([c] (cond
         (instance? com.jme3.math.ColorRGBA c) c
         (sequential? c) (apply color c)
         (number? c) (color c c c)
         :else (util/arg-err
                 "Cannot convert" c "to color.")))
  ([r g b] (color r g b 1.0))
  ([r g b a]
   (com.jme3.math.ColorRGBA. r g b a)))

;; =====
;; Material
;; =====
(defn material? [obj] (instance? Material obj))

(extend-type Material
  Configurable
  (configure [this params]
    (configure-helper
      params param
      :colors (doseq [[color-name color] param]
                (.setColor this color-name color))
      :textures (doseq [[texture-name texture] param]
                  (.setTexture this texture-name texture))
      )))

(defn ^Material material
  "Create a Material
  
  Options:
  "
  {:arglists '([app path & options]
               [asset-manager path & options])}
  [app path & {:as options}]
  (conf-int (Material. (asset-manager app) path)
            options))

(defn lit-material
  ""
  [app texture-map]
  (let [am (asset-manager app)
        textures (for [[k path] texture-map]
                   [(case k
                      :diffuse "DiffuseMap"
                      :specular "SpecularMap"
                      :normal "NormalMap")
                    (.loadTexture am ^String path)])]
    (material app "Common/MatDefs/Light/Lighting.j3md"
              :textures textures)))

(defn simple-material
  ""
  [app c]
  (material app "Common/MatDefs/Misc/Unshaded.j3md"
            :colors [["Color" (color c)]]))

;; =====
;; Geometry
;; =====
(extend-type Spatial
  Configurable
  (configure [s params]
    (configure-helper 
      params param
      :translation (.setLocalTranslation s (jvector param))
      :scale (.setLocalScale s
               ^Vector3f (jvector3 param))
      :material (.setMaterial s param)
      )))

(defn mesh? [obj] (instance? Mesh obj))

(defn ^Geometry geometry
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
(defn ^Node node
  ""
  ([] (Node. (str (gensym))))
  ([s] (Node. (name s))))

;; =====
;; Shape
;; =====
(defn ^Box box
  "Create a Box
  Should be treated as a mesh.
  
  The box is extended x y and z in each direction
  from the center.
  "
  [x y z] (Box. x y z))

(defn ^Sphere sphere
  "Create a Sphere
  Should be treated as a mesh.
  "
  [z-samples radial-samples radius]
  (Sphere. z-samples radial-samples radius))

(defn ^Sphere line
  "Create a Line
  Should be treated as a mesh.
  
  start and end 
  "
  ([] (Line.))
  ([start end]
   (Line. (jvector start) (jvector end))))
