; Ideas:
; - garbage collect local state on decomponent delete
;   - if a desc contains :fx/extend-path, dissoc the extended
;     path from *context
(ns cljfx.decomponent-test.demo
  (:require [cljfx.api :as fx]
            [cljfx.decomponent-test.button-pane :as button-pane]))

;; Initial State

(defn init-state [] {::dynamic-ids []
                     ::flip-layout false
                     ::decomponents {}})

(defn button-pane-path [id]
  [::button-pane id])

(defn all-button-pane-paths [context path]
  (mapv #(conj path (button-pane-path %))
        (fx/sub context ::dynamic-ids)))

;; Effects

(def effects {::gen-button-pane-id
              (fn [m dispatch!]
                (let [the-effect (gensym :button-pane-id)]
                  (dispatch!
                    (assoc m :id the-effect))))})

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-button-panes
  [{:keys [fx/context id] :as m}]
  (if id
    {:context (fx/swap-context context update ::dynamic-ids conj id)}
    ; generate a button id by elaborately calling
    ; an effect that calls this handler again
    {::gen-button-pane-id (select-keys m [:event/type])}))

(defmethod handler ::less-button-panes
  [{:keys [fx/context] :as m}]
  {:context (fx/swap-context context update ::dynamic-ids
                             #(or (when (seq %) (pop %)) []))})

(defmethod handler ::flip-layout
  [{:keys [fx/context] :as m}]
  {:context (fx/swap-context context update ::flip-layout not)})

(defmethod handler :default
  [m]
  (println "No handler: " (:event/type m)))

;; Views

(defn dynamic-button-panes [{:keys [fx/context]}]
  {:fx/type :scroll-pane
   :fit-to-width true
   :fit-to-height true
   :content
   {:fx/type (if (fx/sub context ::flip-layout) :v-box :h-box)
    :children (mapv #(do
                       {:fx/type button-pane/view
                        :fx/extend-path (button-pane-path %)})
                    (fx/sub context ::dynamic-ids))}})

(defn sum-clicks [context path]
  (prn "sum-clicks top-level" path)
  (reduce #(+ %1 (fx/sub context button-pane/sum-clicks %2))
          0
          (fx/sub context all-button-pane-paths path)))

(defn view [{:keys [fx/context fx/path] :as m}]
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
                   {:fx/type :label
                    :text (str "Grand total clicks: " (fx/sub context sum-clicks path))}
                   {:fx/type dynamic-button-panes}]}}})

;; Main app

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app)))

(def *context
  (atom (fx/create-context (init-state))))

(def app
  (fx/create-app *context
    :decomponents `#{button-pane/decomponent}
    :decomponent-root ::decomponents
    :event-handler handler
    :desc-fn (fn [_]
               {:fx/type view})
    :effects effects))
