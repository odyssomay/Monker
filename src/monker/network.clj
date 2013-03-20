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
