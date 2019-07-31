(ns cljfx.decomponent-test.button
  (:require [cljfx.api :as fx]))

;; Events

(defmulti handler :event/type)
(defmethod handler ::clicked
  [{:keys [fx/context fx/root clicked on-action from-effect] :as m}]
  {:pre [root]}
  (if from-effect
    (prn "REBOUND" root)
    (cond->
      {:context (fx/swap-context context update ::clicked (fnil inc 0))
       ; Note: :fx/root must be passed manually to effects
       ::log-click (select-keys m [:clicked :fx/root])}
      on-action (assoc :dispatch (assoc on-action :total-clicked clicked)))))

;; Views

(defn view
  "Main view to show a button with internal state."
  [{:keys [fx/context fx/root on-action] :as m}]
  (prn "root" root)
  (prn "context root" (:cljfx.context/root context))
  (let [clicked (or (fx/sub context ::clicked) 0)]
    {:fx/type :button
     ; :fx/root implicitly passed to events
     :on-action {:event/type ::clicked
                 :clicked clicked
                 :on-action on-action}
     :text (str "Clicked x" clicked)}))

(def effects
  {::log-click (fn [{:keys [clicked fx/root]} dispatch!]
                 {:pre [root]}
                 (prn "LOG CLICK:" root "clicked!" clicked)
                 (dispatch! {:event/type ::clicked
                             :from-effect true}))})

(def decomponent
  {:effects effects
   :event-handler-map (dissoc (methods handler) :default)})

;; Main app

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app)))

(def *context
  (atom (fx/create-context {})))

(def app
  (fx/create-app *context
    :decomponents `#{decomponent}
    :decomponent-root ::decomponents
    :event-handler handler
    :desc-fn (fn [_]
               {:fx/type :stage
                :showing true
                :always-on-top true
                :width 600
                :height 500
                :scene {:fx/type :scene
                        :root {:fx/type view
                               :fx/root [::decomponents ::my-button]}}})))
