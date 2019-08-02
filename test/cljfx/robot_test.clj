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


(defn attach []
  @(fx/on-fx-thread)
  (testfx/exec
    {:testfx/op :fx-robot/target-window
     :window-index 0})
  (let [target (testfx/exec
                 {:testfx/op :fx-robot/target-window})]
    @(fx/on-fx-thread
       ((testfx/setter :stage :always-on-top true)
        target))))

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

(defn lookup-query [s]
  {:testfx/op :fx-robot/lookup
   :query s})

(defn lookup-predicate [p]
  {:testfx/op :fx-robot/lookup
   :predicate p})

(defn backspace
  ([] (backspace 1))
  ([n]
   {:testfx/op :fx-robot/erase-text
    :amount n}))

(defn verify-text-field-query [query expected]
  (is (= expected
         (-> (testfx/exec
               (lookup-query query))
             ((testfx/getter :text-field :text))))))

(deftest text-field-type-and-verify-test
  (testfx/with-robot
    (start-text-field-example)
    (attach)

    (testfx/exec
      (click ".text-field")
      (write "hello world"))
    (verify-text-field-query ".text-field"
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
    (verify-text-field-query ".text-field"
      "ab")

    (testfx/exec
      (write [:a :b :c :d]))
    (verify-text-field-query ".text-field"
      "ababcd")

    (teardown)))

(deftest e01-basic-test
  (testfx/with-robot
    (require 'e01-basic :reload)
    (attach)
    (testfx/exec
      (click ".text-field")
      (write "hello world"))
    (verify-text-field-query ".text-field"
      "hello world")
    (teardown)))

(defn lookup-node-query-from [^org.testfx.service.query.NodeQuery from ^String query]
  (.lookup from query))

(defn text-field-next-to-label [query]
  (testfx/resolve-node-query
    (lookup-node-query-from
      (testfx/exec
        {:testfx/op :fx-robot/from
         :parent-nodes (-> (testfx/exec (lookup-query query))
                           ((testfx/getter :node :parent))
                           vector)})
      ".text-field")))

(defn click-text-field-next-to-label [query]
  {:testfx/op :fx-robot/click-on
   :bounds (-> (testfx/exec
                 {:testfx/op :fx-robot/bounds
                  :node (text-field-next-to-label query)})
               testfx/resolve-bounds-query)})

(defn verify-text-field-next-to-label [query expected]
  (is (= expected
         (-> (text-field-next-to-label query)
             ((testfx/getter :text-field :text))))))

(deftest e02-fn
  (testfx/with-robot
    (require 'e02-fn :reload)
    (attach)
    (testfx/exec
      (click-text-field-next-to-label "First Name")
      (write "hello"))
    (verify-text-field-next-to-label "First Name"
      "hello")

    (testfx/exec
      (click-text-field-next-to-label "Last Name")
      (write "world"))
    (verify-text-field-next-to-label "Last Name"
      "world")

    (teardown)))

(deftest e03-map-event-handler
  (testfx/with-robot
    (require 'e03-map-event-handler :reload)
    (attach)
    (testfx/exec
      (click ".text-field")
      (write "hello world"))
    (verify-text-field-query ".text-field"
      "hello world")
    (teardown)))

(defn verify-label-text-by-label-prefix [^String starts-with expected]
  (is (= expected
         (-> (testfx/exec
               (lookup-predicate (fn [p]
                                   (when (instance? javafx.scene.control.Label p)
                                     (-> p
                                         ^String ((testfx/getter :label :text))
                                         (.startsWith starts-with))))))
             ((testfx/getter :label :text))))))

(deftest e04-state-with-context
  (testfx/with-robot
    (require 'e04-state-with-context :reload)
    (attach)
    (testfx/exec
      (click-text-field-next-to-label "First Name")
      (backspace 4)
      (write "hello"))
    (verify-text-field-next-to-label "First Name"
      "hello")

    (testfx/exec
      (click-text-field-next-to-label "Last Name")
      (backspace 9)
      (write "world"))
    (verify-text-field-next-to-label "Last Name"
      "world")

    (verify-label-text-by-label-prefix "You are "
      "You are hello world!")

    (teardown)))

(deftest e05-fn-fx-like-state
  (testfx/with-robot
    (require 'e05-fn-fx-like-state :reload)
    (attach)
    (testfx/exec
      (click-text-field-next-to-label "First Name")
      (backspace 4)
      (click-text-field-next-to-label "Last Name")
      (backspace 9))

    (verify-label-text-by-label-prefix "You are "
      "You are very mysterious!")
    (verify-label-text-by-label-prefix "Please,"
      "Please, introduce yourself:")


    (testfx/exec
      (click-text-field-next-to-label "First Name")
      (write "hello"))

    (verify-text-field-next-to-label "First Name"
      "hello")
    (verify-label-text-by-label-prefix "You are "
      "You are hello !")

    (testfx/exec
      (click-text-field-next-to-label "Last Name")
      (write "world"))

    (verify-text-field-next-to-label "Last Name"
      "world")
    (verify-label-text-by-label-prefix "You are "
      "You are hello world!")

    (teardown)))
