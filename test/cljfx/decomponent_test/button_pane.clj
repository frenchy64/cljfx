(ns cljfx.decomponent-test.button-pane
  "Decomponent for a pane of dynamically added buttons.
  
  [[view]] is the main view.
  [[decomponent]] is the decomponent configuration.
  
  Implementation details of decomponent and views are subject to change."
  (:require [cljfx.api :as fx]
            [cljfx.decomponent-test.button :as button]
            [cljfx.decomponent :as dc]))

;; Effects

(defn- create-button-id []
  (keyword "cljfx.decomponent-test.button-pane.random-decomponent"
           (str (java.util.UUID/randomUUID))))

(def ^:private effects
  {::gen-button-id
   (fn [m dispatch!]
     (dispatch!
       (assoc m :button-id (create-button-id))))})

;; Event handler

(defn- button-clicks [context id]
  (-> (fx/sub context ::buttons)
      (get-in [id :clicked] 0)))

(def ^:private event-handler-map
  {::more-buttons
   (fn [{:keys [fx/context button-id] :as m}]
     (if button-id
       (do
         (assert (keyword? button-id))
         {:context (-> context
                       (fx/swap-context assoc-in [::buttons button-id] {:clicked 0})
                       ; context preserves root
                       (fx/swap-context update ::dynamic-buttons-order (fnil conj []) button-id))})
       ; get an effect to generate a button id
       {::gen-button-id (select-keys m [:event/type])}))

   ::less-buttons
   (fn [{:keys [fx/context fx/root] :as m}]
     {:pre [root]}
     (let [[old-dynamic-buttons-order removed-button] ((juxt identity peek) (fx/sub context ::dynamic-buttons-order))
           new-dynamic-buttons-order (if (seq old-dynamic-buttons-order)
                                       (pop old-dynamic-buttons-order)
                                       [])]
       {:context (cond-> (fx/swap-context context assoc ::dynamic-buttons-order new-dynamic-buttons-order)
                   removed-button
                   (-> (fx/swap-context update ::buttons dissoc removed-button)
                       (fx/swap-context update ::decomponents dissoc removed-button)
                       (fx/swap-context update ::total-clicks (fnil - 0)
                                        (fx/sub context button-clicks removed-button))))}))

   ::button-clicked
   (fn [{:keys [fx/context button-id on-clicked] :as m}]
     {:pre [(keyword? button-id)]}
     (let [total-clicks ((fnil inc 0) (fx/sub context ::total-clicks))]
       (cond->
         {:context (-> context
                       ; relative-button-root is the buttons _key_ in ::buttons
                       (fx/swap-context update-in [::buttons button-id :clicked] (fnil inc 0))
                       (fx/swap-context assoc ::total-clicks total-clicks))}
         on-clicked (assoc :dispatch (assoc on-clicked :total-clicks total-clicks)))))})

;; Views

(defn- create-button [{:keys [fx/root id on-clicked]}]
  {:fx/type button/view
   :fx/root (into root [::decomponents id])
   :on-clicked {:event/type ::button-clicked
                :fx/root root
                :button-id id
                :on-clicked on-clicked}})

(defn- dynamic-buttons-view [{:keys [fx/context fx/root on-clicked]}]
  {:pre [root]}
  {:fx/type :v-box
   :children (mapv #(do
                      {:fx/type create-button
                       :id %
                       :on-clicked on-clicked})
                   (fx/sub context ::dynamic-buttons-order))})

(defn- summarize-buttons [{:keys [fx/context]}]
  (let [sum (or (fx/sub context ::total-clicks) 0)]
    {:fx/type :label
     :text (str "Sum: " sum)}))

(defn view
  "Main view of button panes. Requires an :fx/root 
  and a decomponent dependency on [[decomponent]].
  
  Takes an optional event :on-clicked that is called when
  a button is clicked. Event receives an additional
  :total-clicks entry which is the total clicks so
  far on this pane."
  [{:keys [fx/root on-clicked]}]
  {:fx/type :v-box
   :spacing 10
   :children [{:fx/type :label
               :text "Buttons with static ids"}
              {:fx/type :h-box
               :children [{:fx/type create-button
                           :id ::first-button
                           :on-clicked on-clicked}
                          {:fx/type create-button
                           :id ::second-button
                           :on-clicked on-clicked}]}
              {:fx/type :label
               :text "Buttons with dynamic ids"}
              {:fx/type :h-box
               :children [{:fx/type :button
                           :on-action {:event/type ::less-buttons}
                           :text (str "Less buttons")}
                          {:fx/type :button
                           :on-action {:event/type ::more-buttons}
                           :text (str "More buttons")}]}
              {:fx/type dynamic-buttons-view
               :on-clicked on-clicked}
              {:fx/type summarize-buttons}]})

;; Decomponent

(def decomponent
  "Decomponent for button pane. Implementation is private and subject to change."
  {:decomponents `#{button/decomponent}
   :effects effects
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
  (let [ids (take 3 (repeatedly gensym))]
    (fx/create-app *context
      :decomponents `#{decomponent}
      :event-handler #(println "No handler: " (:event/type %))
      :desc-fn (fn [_]
                 {:fx/type :stage
                  :showing true
                  :always-on-top true
                  :width 900
                  :height 400
                  :scene {:fx/type :scene
                          :root {:fx/type :scroll-pane
                                 :fit-to-width true
                                 :fit-to-height true
                                 :content
                                 {:fx/type :h-box
                                  :children (mapv #(do
                                                     {:fx/type view
                                                      :fx/root [%]})
                                                  ids)}}}}))))
)
