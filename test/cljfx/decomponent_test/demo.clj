(ns cljfx.decomponent-test.demo
  (:require [cljfx.api :as fx]
            [cljfx.decomponent :as dc]
            [cljfx.decomponent-test.button-pane :as button-pane]
            [cljfx.decomponent-test.dc-utils :as dc-utils]))

(defn- create-pane-id []
  (keyword "cljfx.decomponent-test.demo.random-decomponent"
           (str (java.util.UUID/randomUUID))))

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-button-panes
  [{:keys [fx/context] :as m}]
  (let [button-pane-id (create-pane-id)]
    {:context (-> context
                  (fx/swap-context assoc-in [::panes button-pane-id] {:clicked 0})
                  (fx/swap-context update ::pane-order (fnil conj []) button-pane-id))}))

(defmethod handler ::less-button-panes
  [{:keys [fx/context] :as m}]
  (let [pane-order (fx/sub context ::pane-order)
        new-pane-order (if (seq pane-order)
                         (pop pane-order)
                         [])]
    {:context (cond-> (fx/swap-context context assoc ::pane-order new-pane-order)
                (seq pane-order)
                (-> (fx/swap-context update ::decomponents dissoc (peek pane-order))
                    (fx/swap-context update ::panes dissoc (peek pane-order))))}))

(defmethod handler ::flip-layout
  [{:keys [fx/context] :as m}]
  {:context (fx/swap-context context update ::flip-layout not)})

(defmethod handler ::clicked
  [{:keys [fx/context pane-id]}]
  {:pre [pane-id]}
  (prn "::clicked demo")
  {:context (-> context
                (fx/swap-context update-in [::panes pane-id :clicked] (fnil inc 0))
                (fx/swap-context update ::total-clicks (fnil inc 0)))})

(defmethod handler :default
  [m]
  (println "No handler: " (:event/type m)))

;; Views

(defn- clicked-event [root pane-id]
  {:event/type ::clicked
   :fx/root root
   :pane-id pane-id})

(defn dynamic-button-panes [{:keys [fx/context fx/root]}]
  {:fx/type :scroll-pane
   :fit-to-width true
   :fit-to-height true
   :content
   {:fx/type (if (fx/sub context ::flip-layout) :v-box :h-box)
    :children (mapv #(do
                       {:fx/type button-pane/view
                        :fx/root (into root [::decomponents %])
                        :on-clicked (clicked-event root %)})
                    (fx/sub context ::pane-order))}})

(defn total-clicks-view [{:keys [fx/context] :as m}]
  {:fx/type :label
   :text (str "Grand total clicks: " (or (fx/sub context ::total-clicks) 0))})

(defn view [_]
  {:fx/type :stage
   :showing true
   :always-on-top true
   :width 600
   :height 500
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children
                  [{:fx/type :h-box
                    :children [{:fx/type :button
                                :on-action {:event/type ::less-button-panes}
                                :text (str "Less button panes")}
                               {:fx/type :button
                                :on-action {:event/type ::more-button-panes}
                                :text (str "More button panes")}
                               {:fx/type :button
                                :on-action {:event/type ::flip-layout}
                                :text (str "Flip layout")}]}
                   {:fx/type total-clicks-view}
                   {:fx/type dynamic-button-panes}]}}})

;; Main app

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app)))

(def *context
  (atom (fx/create-context {})))

(def app
  (fx/create-app *context
    :decomponents `#{button-pane/decomponent}
    :decomponent-root ::decomponents
    :event-handler handler
    :desc-fn (fn [_]
               {:fx/type view})))
