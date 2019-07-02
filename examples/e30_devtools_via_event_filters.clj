; Author: Ambrose Bonnaire-Sergeant
(ns e30-devtools-via-event-filters
  (:require [cljfx.api :as fx]
            [cljfx.ext.node :as fx.ext.node])
  (:import [javafx.scene.input MouseEvent]
           [javafx.scene Node]
           [javafx.event Event]))

;; Setting an event filter on a node allows that node to
;; intercept any events intended for its children.
;; 
;; Wrap `fx.ext.node/with-event-filter-props` around any node
;; as below to set an :event-filter prop. Note, wrapped node
;; must implement Node, so it cannot be a Stage/Scene.
;;
;; This example highlights hovered nodes by
;; setting an event filter on the root node that intercepts
;; MouseEvent's sent to inner nodes.

(set! *warn-on-reflection* true)

(def *context
  (atom (fx/create-context
          {:current-node nil})))

(def mouse-over-css-class "mouse-over-highlight")

(defmulti handle-event :event/type)

;; set :current-node to the most specific node node under the cursor
;; and highlight it 
(defmethod handle-event ::on-event-filter [{:keys [fx/context ^Event fx/event]}]
  (when (instance? MouseEvent event)
    (let [^Node target (.getTarget event)
          event-type (.getEventType event)]
      (when (instance? Node target)
        ; extra logic to disable highlighting for parents of `target`
        (if (#{MouseEvent/MOUSE_EXITED
               MouseEvent/MOUSE_EXITED_TARGET}
              event-type)
          (do (-> target .getStyleClass (.remove mouse-over-css-class))
              {:context (fx/swap-context context update :current-node #(when (not= % target) %))})
          (let [^Node hovered-node (fx/sub context :current-node)
                _ (when (and hovered-node (not= hovered-node target))
                    (-> hovered-node .getStyleClass (.remove mouse-over-css-class)))
                _ (when (not= hovered-node target)
                    (when-not (-> target .getStyleClass (.contains mouse-over-css-class))
                      (-> target .getStyleClass (.add mouse-over-css-class))))]
            {:context (fx/swap-context context assoc :current-node target)}))))))

(defn root-view [{:keys [fx/context]}]
  {:fx/type :stage
   :showing true
   :width 600
   :height 500
   :scene {:fx/type :scene
           :stylesheets #{"devtools.css"}
           :root {:fx/type fx.ext.node/with-event-filter-props
                  ;; add an event filter to the root node
                  :props {:event-filter {:event/type ::on-event-filter
                                         :fx/sync true}}
                  :desc {:fx/type :v-box
                         :children
                         [{:fx/type :label
                           :text (str "Current node: " (some-> (fx/sub context :current-node)
                                                               class
                                                               .getSimpleName))}
                          {:fx/type :label
                           :text (str "Has CSS classes: " (some-> ^Node (fx/sub context :current-node)
                                                                  .getStyleClass
                                                                  vec))}
                          ; mouse over these nodes to highlight them
                          {:fx/type :split-pane
                           :divider-positions [0.5]
                           :items [{:fx/type :v-box
                                    :padding 50
                                    :children [{:fx/type :split-pane
                                                :divider-positions [0.5]
                                                :items [{:fx/type :v-box
                                                         :padding 50
                                                         :children [{:fx/type :h-box
                                                                     :children [{:fx/type :label
                                                                                 :text "left 1"}]}
                                                                    {:fx/type :label
                                                                     :text "left 2"}]}
                                                        {:fx/type :h-box
                                                         :padding 50
                                                         :children [{:fx/type :label
                                                                     :text "right"}]}]}
                                               {:fx/type :label
                                                :text "left 2"}]}
                                   {:fx/type :h-box
                                    :padding 50
                                    :children [{:fx/type :label
                                                :text "right"}]}]}]}}}})

(def app
  (fx/create-app *context
    :event-handler handle-event
    :desc-fn (fn [_]
               {:fx/type root-view})))
