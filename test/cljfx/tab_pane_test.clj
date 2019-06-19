(ns cljfx.tab-pane-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [cljfx.context :as context]
            [cljfx.api :as fx]
            [cljfx.defaults :as defaults])
  (:import (javafx.scene.control Tab TabPane)))

(def get-pane-tabs #(.getTabs ^TabPane %))
(def get-tab-id #(.getId ^Tab %))

(defn setup-handler [*context event-handler]
  (-> event-handler
      (fx/wrap-co-effects
        (defaults/fill-co-effects {} *context))
      (fx/wrap-effects
        (defaults/fill-effects {} *context))
      fx/wrap-async))

(deftest on-tabs-changed-interacts-well-with-tabs-prop
  ; Adds and rearranges tabs:
  ; 1. []      <= init
  ; 2. [1 2]   <= advance component
  ; 3. [3 2 1] <= advance component
  ; 4. [1 2 3] <= manual .setAll
  ; 5. [1 2]   <= manual .setAll
  ; 6. [1]     <= manual .setAll
  (is
    (= :ok
       (let [*context (atom (fx/create-context
                              {:tab-order []}))
             context-tab-order #(fx/sub @*context :tab-order)
             event-handler (fn [{:keys [event/type fx/context fx/event]}]
                             {:pre [context]}
                             (case type
                               ::on-tabs-changed
                               {:context (fx/swap-context context assoc :tab-order (mapv get-tab-id event))}))
             opts {:fx.opt/map-event-handler (setup-handler *context event-handler)}
             tabs-desc (mapv #(do
                                {:fx/type :tab
                                 :fx/key (str %)
                                 :id (str %)})
                             (range 1 4))
             tab-pane-desc {:fx/type :tab-pane
                            :tabs []
                            :on-tabs-changed {:event/type ::on-tabs-changed
                                              :fx/sync true}}
             tab-order (fn [tpc] (mapv get-tab-id (get-pane-tabs (fx/instance tpc))))
             tpc (fx/create-component (assoc tab-pane-desc :tabs [])
                                      opts)
             ; 1.
             _ (is (= []
                      (tab-order tpc)
                      (context-tab-order)))
             tpc (fx/advance-component
                   tpc
                   (assoc tab-pane-desc :tabs (take 2 tabs-desc))
                   opts)
             ; 2.
             _ (is (= ["1" "2"] (tab-order tpc)))
             tpc (fx/advance-component
                   tpc
                   (assoc tab-pane-desc :tabs (reverse tabs-desc))
                   opts)
             ; 3.
             _ (is (= ["3" "2" "1"] (tab-order tpc)))
             ^javafx.collections.ObservableList ol (get-pane-tabs (fx/instance tpc))
             _ (.setAll ol ^java.util.Collection (reverse ol))
             ; 4.
             _ (is (= ["1" "2" "3"]
                      (tab-order tpc)
                      (context-tab-order)))
             _ (.setAll ol ^java.util.Collection (vec (take 2 ol)))
             ; 5.
             _ (is (= ["1" "2"]
                      (tab-order tpc)
                      (context-tab-order)))
             _ (.setAll ol ^java.util.Collection (vec (take 1 ol)))
             ; 6.
             _ (is (= ["1"]
                      (tab-order tpc)
                      (context-tab-order)))
             ]
         :ok))))
