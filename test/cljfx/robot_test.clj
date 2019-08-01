(ns cljfx.robot-test
  (:require [cljfx.api :as fx]
            [cljfx.testfx :as testfx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [javafx.geometry Point2D]
           javafx.scene.input.KeyCode))

(set! *warn-on-reflection* true)

(def state (atom {}))

(fx/on-fx-thread
  (let [cmp (fx/create-component
              {:fx/type :stage
               :showing true
               :always-on-top true
               :scene {:fx/type :scene
                       :root {:fx/type :text-field}}})
        _ (swap! state assoc :cmp cmp)
        robot (testfx/robot)
        _ (swap! state assoc :robot robot)
        ^javafx.stage.Window window (fx/instance cmp)]
    ))

(Thread/sleep 1000)

(fx/on-fx-thread
  (let [{:keys [cmp ^FxRobot robot]} @state
        ^javafx.stage.Window window (fx/instance cmp)
        point-query (.point robot
                            (-> window
                                .getScene
                                .getRoot))]
    (testfx/exec robot
                 {:testfx/op :click-robot/click-on
                  :point-query point-query
                  :motion :direct
                  :buttons :primary})
    ;(.targetWindow robot ^javafx.stage.Window (fx/instance cmp))
    #_
    (.clickOn robot point-query (testfx/coerce-motion :direct)
              ^"[Ljavafx.scene.input.MouseButton;"
              (into-array javafx.scene.input.MouseButton [javafx.scene.input.MouseButton/PRIMARY]))
    (.type robot ^"[Ljavafx.scene.input.KeyCode;" (into-array javafx.scene.input.KeyCode [(KeyCode/valueOf "A")]))))
