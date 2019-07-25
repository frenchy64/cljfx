(ns cljfx.context-test.empty-context
  (:require [cljfx.api :as fx]))

;; Initial State

(defn init-state [] {#_#_::clicked 0})

;; Views

(defn button-with-state
  "Use this view to show a button"
  [{:keys [fx/context] :as m}]
  (let [clicked (fx/sub context ::clicked)]
    {:fx/type :button
     :on-action {:event/type ::clicked
                 :clicked clicked}
     :text (str "Clicked x" (or clicked 0))}))

(defn view [_]
  {:fx/type :stage
   :showing true
   :always-on-top true
   :width 600
   :height 500
   :scene {:fx/type :scene
           :root {:fx/type button-with-state}}})

; Effects

(def effects {::log-click (fn [{:keys [clicked fx/path]} dispatch!]
                            (prn path "clicked!" (or clicked 0)))})

; Handlers

(defmulti handler :event/type)
(defmethod handler ::clicked
  [{:keys [fx/context clicked]}]
  {:context (fx/swap-context context update ::clicked (fnil inc 0))
   ::log-click {:clicked clicked}})

;; Main app

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app)))

(def *context
  (atom (fx/create-context (init-state))))

(def app
  (fx/create-app *context
    :event-handler handler
    :desc-fn (fn [_]
               {:fx/type view})
    :effects effects))
