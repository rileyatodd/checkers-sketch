(ns checkers.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:require [checkers.dynamic :as dynamic])
  (:require [genartlib.util :as u])
  (:gen-class))

(q/defsketch checkers-sketch
  :title "Rotating checkers"
  :size [dynamic/sketch-width dynamic/sketch-height]
  ; setup function called only once, during sketch initialization.
  :setup dynamic/setup
  ; update-state is called on each iteration before draw-state.
  :update dynamic/update-state
  :draw dynamic/draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn refresh []
  (use :reload 'checkers.dynamic)
  (.redraw checkers-sketch))

(defn get-applet []
  checkers-sketch)