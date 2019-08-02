(ns cljfx.robot-test
  (:require [cljfx.api :as fx]
            [cljfx.testfx :as testfx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce]
            [clojure.test :refer :all])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [javafx.geometry Point2D]
           [javafx.stage Window]
           javafx.scene.input.KeyCode))

(set! *warn-on-reflection* true)


#_
{:testfx/op :fx-robot/point
 :node (-> stage
           ((testfx/getter :stage :scene))
           ((testfx/getter :scene :root)))}

(defn attach []
  @(fx/on-fx-thread)
  (testfx/exec
    {:testfx/op :fx-robot/target-window
     :window-index 0}))

(defn teardown []
  (let [target (testfx/exec
                 {:testfx/op :fx-robot/target-window})]
    @(fx/on-fx-thread
       ((testfx/setter :stage .close) target))))

(defn start-text-field-example []
  (fx/on-fx-thread
    (fx/create-component
      {:fx/type :stage
       :showing true
       :always-on-top true
       :scene {:fx/type :scene
               :root {:fx/type :text-field}}})))

(defn click [query]
  {:testfx/op :fx-robot/click-on
   :query query})

(defn write [s]
  {:testfx/op :fx-robot/type
   :key-codes (map (comp keyword
                         (some-fn {" " :space} identity)
                         #(if ((some-fn keyword? symbol?) %)
                            (name %)
                            (str %)))
                   (if ((some-fn keyword? symbol?) s)
                     (name s)
                     s))})

(defn lookup [s]
  {:testfx/op :fx-robot/lookup
   :query s})

(defn backspace []
  {:testfx/op :fx-robot/erase-text})

(defn verify-text-field [selector expected]
  (is (= expected
         (-> (testfx/exec
               (lookup selector))
             ((testfx/getter :text-field :text))))))

(deftest text-field-type-and-verify-test
  (testfx/with-robot
    (start-text-field-example)
    (attach)

    (testfx/exec
      (click ".text-field")
      (write "hello world"))
    (verify-text-field ".text-field"
      "hello world")

    (teardown))
  (testfx/with-robot
    (start-text-field-example)
    (attach)

    (testfx/exec
      (click ".text-field")
      (write (repeat 2 :a))
      (backspace)
      (write :b))
    (verify-text-field ".text-field"
      "ab")

    (testfx/exec
      (write [:a :b :c :d]))
    (verify-text-field ".text-field"
      "ababcd")

    (teardown)))

(deftest e01-basic-test
  (testfx/with-robot
    (require 'e01-basic :reload)
    (attach)
    (testfx/exec
      (click ".text-field")
      (write "hello world"))
    (verify-text-field ".text-field"
      "hello world")
    (teardown)))
