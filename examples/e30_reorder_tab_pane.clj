(ns e30-reorder-tab-pane
  (:require [cljfx.api :as fx]
            [cljfx.prop :as prop]
            [cljfx.lifecycle :as lifecycle]
            [cljfx.mutator :as mutator])
  (:import [java.util UUID Collection]
           [javafx.scene.control TabPane Tab]))

(def *state
  (atom []))

(fx/mount-renderer
  *state
  (fx/create-renderer
    :opts {:fx.opt/map-event-handler
           (fn [e]
             (case (:event/type e)
               ::add-tab (swap! *state conj (str (UUID/randomUUID)))
               ::sort-tabs (swap! *state (comp vec sort))
               ::set-tabs (swap! *state (constantly (mapv #(.getId ^Tab %) (:fx/event e))))))}
    :middleware (fx/wrap-map-desc
                  (fn [tab-ids]
                    {:fx/type :stage
                     :width 960
                     :height 540
                     :showing true
                     :scene {:fx/type :scene
                             :root {:fx/type :v-box
                                    :children [{:fx/type :h-box
                                                :children
                                                [{:fx/type :button
                                                  :text "Add tab"
                                                  :on-action {:event/type ::add-tab}}
                                                 {:fx/type :button
                                                  :text "Sort tabs"
                                                  :on-action {:event/type ::sort-tabs}}]}
                                               {:fx/type :label
                                                :text (str (mapv #(subs % 0 3) tab-ids))}
                                               {:fx/type :tab-pane
                                                :style {:-fx-open-tab-animation "NONE"
                                                        :-fx-close-tab-animation "NONE"}
                                                :tab-closing-policy :all-tabs
                                                :tab-drag-policy :reorder
                                                :on-tabs-changed {:event/type ::set-tabs}
                                                :tabs (for [id tab-ids]
                                                        {:fx/type :tab
                                                         :fx/key id
                                                         :id id
                                                         :text (subs id 0 3)})}]}}}))))
