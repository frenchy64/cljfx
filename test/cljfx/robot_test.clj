(ns cljfx.robot-test
  (:require [cljfx.api :as fx]
            [cljfx.testfx :as testfx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce]
            [clojure.test :refer :all])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [javafx.geometry Point2D]
           javafx.scene.input.KeyCode))

(set! *warn-on-reflection* true)


#_
{:testfx/op :fx-robot/point
 :node (-> stage
           ((testfx/getter :stage :scene))
           ((testfx/getter :scene :root)))}

(deftest text-field-type-and-verify-test
  (let [stage @(fx/on-fx-thread
                 (fx/create-component
                   {:fx/type :stage
                    :showing true
                    :always-on-top true
                    :scene {:fx/type :scene
                            :root {:fx/type :text-field}}}))]
    (testfx/with-robot
      (testfx/exec
        {:testfx/op :fx-robot/target-window
         :window stage}
        {:testfx/op :fx-robot/click-on
         :query ".text-field"}
        {:testfx/op :fx-robot/type
         :key-codes (repeat 2 :a)}
        {:testfx/op :fx-robot/erase-text}
        {:testfx/op :fx-robot/type
         :key-code :b})

      (is (= "ab"
             (-> (testfx/exec
                   {:testfx/op :fx-robot/lookup
                    :query ".text-field"})
                 ((testfx/getter :text-field :text)))))
      (testfx/exec
        {:testfx/op :fx-robot/type
         :key-codes [:a :b :c :d]})

      (is (= "ababcd"
             (-> (testfx/exec
                   {:testfx/op :fx-robot/lookup
                    :query ".text-field"})
                 ((testfx/getter :text-field :text)))))
      )))
