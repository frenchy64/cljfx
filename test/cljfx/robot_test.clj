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


(defn attach
  ([] (attach {:window-index 0}))
  ([m]
   @(fx/on-fx-thread)
   (testfx/exec
     (merge
       {:testfx/op :fx-robot/target-window}
       m))
   (let [target (testfx/exec
                  {:testfx/op :fx-robot/target-window})]
     @(fx/on-fx-thread
        ((testfx/setter :stage :always-on-top true)
         target)))))

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

(defn enter []
  {:testfx/op :fx-robot/type
   :key-code javafx.scene.input.KeyCode/ENTER})

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

(defn lookup-node-query-with-predicate [^org.testfx.service.query.NodeQuery from f]
  (.lookup from (testfx/coerce-predicate f)))

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

(deftest e06-pagination
  (let [test-page (fn [n]
                    (testfx/exec
                      (click n))

                    (Thread/sleep 200)

                    (verify-label-text-by-label-prefix "This is a page "
                      (str "This is a page " n)))]
    (testfx/with-robot
      (require 'e06-pagination :reload)
      (attach)

      (verify-label-text-by-label-prefix "This is a page "
        "This is a page 5")

      (test-page 4)
      (test-page 1)
      (test-page 10)

      (teardown))))

(deftest e07-extra-props
  (let []
    (testfx/with-robot
      (require 'e07-extra-props :reload)
      (attach)

      (apply testfx/exec
             (map click
                  (concat
                    (shuffle
                      ["Border Pane"
                       "Flow Pane"
                       "Grid Pane"
                       "HBox"
                       "Stack Pane"
                       "Tile Pane"
                       "VBox"
                       "Button Bar"])
                    ["Anchor Pane"])))

      (teardown))))

;TODO
#_
(deftest e08-media
  (let [wait-until-video-plays (fn []
                                 (dotimes [_ 20]
                                   )
                                 )]
    (testfx/with-robot
      (require 'e08-media :reload)
      (attach)

      (teardown))))

(deftest e09-todo-app
  (let [check-box-for-label (fn [query]
                              (testfx/resolve-node-query
                                (lookup-node-query-from
                                  (testfx/exec
                                    {:testfx/op :fx-robot/from
                                     :parent-nodes (-> (testfx/exec (lookup-query query))
                                                       ((testfx/getter :node :parent))
                                                       vector)})
                                  ".check-box")))
        click-check-box (fn [query]
                          [{:testfx/op :fx-robot/sleep
                            :milliseconds 200}
                           {:testfx/op :fx-robot/click-on
                            :bounds (-> (testfx/exec
                                          {:testfx/op :fx-robot/bounds
                                           :node (check-box-for-label query)})
                                        testfx/resolve-bounds-query)}
                           {:testfx/op :fx-robot/sleep
                            :milliseconds 200}])
        verify-check-box (fn [query state]
                           (is (= (boolean state)
                                  (-> (check-box-for-label query)
                                      ((testfx/getter :check-box .isSelected))))
                               (str "Incorrect state for " (pr-str query))))]
    (testfx/with-robot
      (require 'e09-todo-app :reload)
      (attach)

      ; starts with milk checked, socks unchecked
      (run! #(apply verify-check-box %)
            {"Buy milk" true
             "Buy socks" false})

      ; uncheck milk
      (testfx/exec
        (click-check-box "Buy milk"))

      (run! #(apply verify-check-box %)
            {"Buy milk" false
             "Buy socks" false})

      ; check milk and socks
      (testfx/exec
         (click-check-box "Buy socks"))

      (run! #(apply verify-check-box %)
            {"Buy milk" false
             "Buy socks" true})

      (testfx/exec
         (click-check-box "Buy milk"))

      (run! #(apply verify-check-box %)
            {"Buy milk" true
             "Buy socks" true})

      ; add water
      (testfx/exec
        (click ".text-field")
        (write "buy water")
        (enter))

      ;flaky?
      ;(run! #(apply verify-check-box %)
      ;      {"Buy milk" true
      ;       "Buy socks" true
      ;       "buy water" false})

      ;; check water, uncheck+check socks and milk (idempotent)
      ;(apply testfx/exec
      ;       (map click-check-box 
      ;            (shuffle
      ;              (apply concat
      ;                     ["buy water"]
      ;                     (repeat 2 ["Buy socks"
      ;                                "Buy milk"])))))

      ;(run! #(apply verify-check-box %)
      ;      {"Buy milk" true
      ;       "Buy socks" true
      ;       "buy water" true})

      (teardown))))

(deftest e10-multiple-windows
  (let []
    (testfx/with-robot
      (require 'e10-multiple-windows :reload)
      @(fx/on-fx-thread)
      (doseq [^Window w (testfx/exec {:testfx/op :fx-robot/list-windows})]
        (attach {:window w})
        (is (== e10-multiple-windows/width (.getWidth w)))
        (is (== e10-multiple-windows/height (.getHeight w)))

        ;TODO
        #_
        (let [[x y] (if (== 0.0 (.getX w))
                      [0 0]
                      [e10-multiple-windows/right
                       e10-multiple-windows/bottom])]
          (verify-label-text-by-label-prefix "Window at "
            (str "Window at [" x ", " y "] with size "
                 e10-multiple-windows/width
                 "x"
                 e10-multiple-windows/height)))
        (teardown)))))

(defn drag-node [node]
  {:testfx/op :fx-robot/drag
   :node node})

(defn drop-to [node x y]
  {:testfx/op :fx-robot/drop-to
   :x x
   :y y})

(deftest e11-mouse-dragging
  (let []
    (testfx/with-robot
      (require 'e11-mouse-dragging :reload)
      (attach)
      (let [circles (-> (testfx/exec
                          (lookup-predicate #(instance? javafx.scene.shape.Circle %)))
                        testfx/resolve-all-node-query
                        vec)]
        (is (= 2 (count circles)))
        (doseq [circle circles]
          (testfx/exec
            (drag-node circle)
            (drop-to circle 500 500))))
      (teardown))
      ))

;FIXME how to click on slider?
(deftest e12-interactive-development
  (let []
    (testfx/with-robot
      (require 'e12-interactive-development :reload)
      (attach)
      (testfx/exec
        (click ".slider")
        )
      (Thread/sleep 200)
      (teardown)
      )))

(deftest e13-re-frame-like-state
  (let [close-button-for-label (fn [pane query]
                                 (testfx/resolve-node-query
                                   (lookup-node-query-with-predicate
                                     (testfx/exec
                                       {:testfx/op :fx-robot/from
                                        :parent-nodes [(-> (testfx/exec (lookup-query query))
                                                           ((testfx/getter :node :parent)))
                                                       (-> (testfx/exec (lookup-query pane))
                                                           ((testfx/getter :node :parent)))
                                                       ]})
                                     #(instance? javafx.scene.layout.StackPane %))))
        click-close-button (fn [pane query]
                             {:testfx/op :fx-robot/click-on
                              :bounds (-> (testfx/exec
                                            {:testfx/op :fx-robot/bounds
                                             :node (close-button-for-label pane query)})
                                          testfx/resolve-bounds-query)})]
    (testfx/with-robot
      (require 'e13-re-frame-like-state :reload)
      (attach)
      ;FIXME pane selection doesn't work, always chooses left pane
      (testfx/exec
        (mapv #(apply click-close-button %)
              (shuffle
                [["Potions" "Applebloom"]
                 ["Ingredients" "Sulfur"]
                 ["Ingredients" "Fireberries"]])))
      (teardown)
      )))
