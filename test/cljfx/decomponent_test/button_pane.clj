(ns cljfx.decomponent-test.button-pane
  (:require [cljfx.api :as fx]
            [cljfx.decomponent-test.button :as button]
            [cljfx.decomponent :as dc]
            [cljfx.decomponent-test.dc-utils :as dc-utils]))

;; Effects

(def effects {::gen-button-id
              (fn [{:keys [fx/root] :as m} dispatch!]
                {:pre [root]}
                (dispatch!
                  (assoc m :button-relative-root (dc/create-decomponent [::decomponents]))))})

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-buttons
  [{:keys [fx/context fx/root button-relative-root] :as m}]
  {:pre [root]}
  (prn "::more-buttons" root button-relative-root)
  (if button-relative-root
    (do
      (assert (vector? button-relative-root))
      {:context (-> context
                    (fx/swap-context assoc-in [::buttons button-relative-root] {:clicked 0})
                    (fx/swap-context update ::dynamic-buttons-order (fnil conj []) button-relative-root))})
    ; get an effect to generate a button id
    {::gen-button-id (select-keys m [:event/type :fx/root])})) ;;FIXME cannot automatically include :fx/root 

(defmethod handler ::less-buttons
  [{:keys [fx/context fx/root] :as m}]
  {:pre [root]}
  (let [dynamic-buttons-order (fx/sub context ::dynamic-buttons-order)
        new-dynamic-buttons-order (if (seq dynamic-buttons-order)
                                    (pop dynamic-buttons-order)
                                    [])]
    {:context (cond-> (fx/swap-context context assoc ::dynamic-buttons-order new-dynamic-buttons-order)
                ; garbage collect old button state
                (seq dynamic-buttons-order)
                (fx/swap-context update ::buttons dissoc (peek dynamic-buttons-order)))}))

(defmethod handler ::button-clicked
  [{:keys [fx/context fx/root relative-button-root] :as m}]
  {:pre [root
         (vector? relative-button-root)]}
  (prn "::button-clicked" root relative-button-root)
  {:context (-> context
                ; relative-button-root is the buttons _key_ in ::buttons
                (fx/swap-context update-in [::buttons relative-button-root :clicked] (fnil inc 0))
                (fx/swap-context update ::total-clicks (fnil inc 0)))})

(defmethod handler :default
  [m]
  (println "No handler: " (:event/type m)))

;; Views

(defn create-button [{:keys [fx/root relative-button-root]}]
  {:fx/type button/view
   :fx/root (into root relative-button-root)
   :on-action {:event/type ::button-clicked
               :fx/root root
               :relative-button-root relative-button-root}})

(defn dynamic-buttons-view [{:keys [fx/context fx/root]}]
  {:pre [root]}
  (prn "dynamic-buttons-view" (fx/sub context ::dynamic-buttons-order))
  {:fx/type :v-box
   :children (mapv #(do
                      {:fx/type button/view
                       :fx/root (into root %)
                       :on-action {:event/type ::button-clicked
                                   :fx/root root
                                   :relative-button-root %}})
                   (fx/sub context ::dynamic-buttons-order))})

(defn summarize-buttons [{:keys [fx/context fx/root]}]
  {:pre [root]}
  (let [sum (or (fx/sub context ::total-clicks) 0)]
    {:fx/type :label
     :text (str "Sum: " sum)}))

(defn view
  "Main view of button panes."
  [{:keys [fx/root]}]
  {:fx/type :v-box
   :spacing 10
   :children [{:fx/type :label
               :text "Buttons with static ids"}
              {:fx/type :h-box
               :children [{:fx/type create-button
                           :relative-button-root [::decomponents ::first-button]}
                          {:fx/type create-button
                           :relative-button-root [::decomponents ::second-button]}]}
              {:fx/type :label
               :text "Buttons with dynamic ids"}
              {:fx/type :h-box
               :children [{:fx/type :button
                           :on-action {:event/type ::less-buttons}
                           :text (str "Less buttons")}
                          {:fx/type :button
                           :on-action {:event/type ::more-buttons}
                           :text (str "More buttons")}]}
              {:fx/type dynamic-buttons-view}
              {:fx/type summarize-buttons}]})

;; Decomponent

; list of decomponent dependencies to garbage collect
(defn delete-decomponent [context root]
  (map #(vector `button/decomponent %)
       (concat (map #(dc/create-decomponent root %) [::first-button ::second-button])
               (map :root (fx/sub context ::dynamic-buttons)))))

(def decomponent
  {:decomponents `#{button/decomponent
                    dc-utils/decomponent}
   :effects effects
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
                               :fx/root [::my-button-pane]}}})))
