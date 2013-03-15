(ns monker.network
  ;(:use [monker.core])
  (:require [clojure.edn :as edn])
  (:import (com.jme3.network
             Network ConnectionListener
             ClientStateListener
             MessageListener)))

(com.jme3.network.serializing.Serializer/registerClass
  monker.StringMessage)

(defn- string-message-listener [s-or-c f]
  (reify MessageListener
    (messageReceived [this source message]
      (f s-or-c
         source
         (edn/read-string
           (.getMessage message))))))

(defn- string-message [obj]
  (monker.StringMessage. (pr-str obj)))

(defn connection-listener [which on-connect on-disconnect]
  (case which
    :server (reify ConnectionListener
              (connectionAdded [this server connection]
                (on-connect server connection))
              (connectionRemoved [this server connection]
                (on-disconnect server connection)))
    :client (reify ClientStateListener
              (clientConnected [this client]
                (on-connect client))
              (clientDisconnected [this client info]
                (on-disconnect client info)))
    ))

(defn server [& {:as args}]
  (let [{:keys [port tcp-port udp-port
                on-message on-connect on-disconnect]
         :or {on-connect (fn [& _])
              on-disconnect (fn [& _])
              on-message (fn [& _])}
         } args
        tcp-port (or tcp-port port 8080)
        [tcp-port udp-port] (if (and tcp-port udp-port)
                              [tcp-port udp-port]
                              [port port])
        s (if udp-port
            (Network/createServer tcp-port udp-port)
            (Network/createServer tcp-port))
        ]
    (.addConnectionListener
      s (connection-listener :server on-connect on-disconnect))
    (.addMessageListener s (string-message-listener s on-message))
  s))

(defn client [& {:as args}]
  (let [{:keys [host port on-connect on-disconnect
                on-message]
         :or {on-connect (fn [& _])
              on-disconnect (fn [& _])
              on-message (fn [& _])}} args
        c (Network/connectToServer host port)]
    (.addClientStateListener
      c (connection-listener :client on-connect on-disconnect))
    (.addMessageListener
      c (string-message-listener c on-message))
    c))

(defn broadcast [server message])

(defn close!
  ([obj] (.close obj))
  ([obj info] (.close obj info)))

;; =====
;; TESTING
;; =====
(defn test-client [port]
  (let [c (client :host "localhost"
                  :port port
                  :on-connect (fn [client]
                                (println "client connected")
                                (.send client (monker.StringMessage.
                                                "{:a 3 :b 4}"))
                                (Thread/sleep 100)
                                (.close client))
                  :on-disconnect (fn [client]
                                   (println "client disconnected")))]
    (println "starting client")
    (.start c)
    )
  )

(defn test-run []
  (let [port 4040
        s (server :port port
                  :on-connect
                  (fn [server connection]
                    (println "connected!"
                             connection))
                  :on-disconnect
                  (fn [server connection]
                    (println "closing server")
                    (.close server))
                  :on-message
                  (fn [server source message]
                    (prn "received" message "from" source)))]
    (try
      (println "starting server")
      (.start s)
      (test-client port)
      (catch Exception e (println "error" e))
      ;(finally (println "closing server") (.close s))
      )))

; (defn test-run-with-app []
;   (application
;     :context-type :headless
;     :init (fn [app]
;             (test-run)
;             (.stop app))))