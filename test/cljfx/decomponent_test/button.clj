(ns cljfx.decomponent-test.button
  (:require [cljfx.api :as fx]))

;; Events

(def ^:private event-handler-map
  {::clicked
   (fn
     [{:keys [fx/context clicked on-clicked from-effect] :as m}]
     (when-not from-effect
       (cond->
         {:context (fx/swap-context context update ::clicks (fnil inc 0))
          ; Note: :fx/root must be passed manually to effects
          ::log-click (select-keys m [:clicked])}
         on-clicked (assoc :dispatch (assoc on-clicked :total-clicks clicked)))))})

;; Views

(defn view
  "Main view to show a button with internal state.
  
  Takes an optional event :on-clicked that is called when
  the button is clicked. Event receives an additional
  :total-clicks entry which is the total clicks so
  far on this pane."
  [{:keys [fx/context on-clicked] :as m}]
  (let [clicked (or (fx/sub context ::clicks) 0)]
    {:fx/type :button
     ; implicitly rooted because immediately passed to :button
     :on-action {:event/type ::clicked
                 :clicked clicked
                 :on-clicked on-clicked}
     :text (str "Clicked x" clicked)}))

;; Effects

(def ^:private effects
  {::log-click (fn [{:keys [clicked]} dispatch!]
                 ; root automatically forwarded to dispatch!
                 (dispatch! {:event/type ::clicked
                             :from-effect true}))})

(def decomponent
  {:effects effects
   :event-handler-map event-handler-map})

;; Test

(comment

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app)))

(def *context
  (atom (fx/create-context {})))

(def app
  (let [ids (take 5 (repeatedly gensym))]
    (fx/create-app *context
      :decomponents `#{decomponent}
      :event-handler #(println "No handler: " (:event/type %))
      :desc-fn (fn [_]
                 {:fx/type :stage
                  :showing true
                  :always-on-top true
                  :width 600
                  :height 500
                  :scene {:fx/type :scene
                          :root {:fx/type :v-box
                                 :children (mapv
                                             #(do
                                                {:fx/type view
                                                 :fx/root [%]})
                                             ids)}}}))))
)
