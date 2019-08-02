(ns cljfx.robot-test
  (:require [cljfx.api :as fx]
            [cljfx.testfx :as testfx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [javafx.geometry Point2D]
           javafx.scene.input.KeyCode))

(set! *warn-on-reflection* true)

(def stage 
  @(fx/on-fx-thread
     (fx/create-component
       {:fx/type :stage
        :showing true
        :always-on-top true
        :scene {:fx/type :scene
                :root {:fx/type :text-field}}})))

(Thread/sleep 1000)

(testfx/with-testfx
  (testfx/exec
    {:testfx/op :fx-robot/click-on
     :point-query {:testfx/op :fx-robot/point
                   :node (-> stage
                             ((testfx/getter :stage :scene))
                             ((testfx/getter :scene :root)))}}
    {:testfx/op :fx-robot/type
     :key-codes (repeat 2 :a)}
    {:testfx/op :fx-robot/erase-text}
    {:testfx/op :fx-robot/type
     :key-code :b}))
