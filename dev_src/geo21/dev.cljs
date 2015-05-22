(ns geo21.dev
    (:require
     [geo21.core]
     [geo21.core-test]
     [figwheel.client :as fw]))

(fw/start {
  :websocket-url "ws://localhost:3449/figwheel-ws"
  :on-jsload (fn []
               ;; (stop-and-start-my app)
               )})
