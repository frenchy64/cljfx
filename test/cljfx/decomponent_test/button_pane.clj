(ns cljfx.decomponent-test.button-pane
  (:require [cljfx.api :as fx]
            [cljfx.decomponent-test.button :as button]
            [cljfx.decomponent :as dc]
            [cljfx.decomponent-test.dc-utils :as dc-utils]))

;; Note:
;; Nested decomponents may result in very long paths.
;; Idea: flatten trees into a :dc/decomponents map at
;; the top-level of the tree, which is a (Map Path DecomponentState).

;; Decomponent paths

(defn- sub-in [context ks]
  {:pre [((every-pred vector? seq) ks)]}
  (get-in (fx/sub context (first ks)) (rest ks)))

(defn- dynamic-buttons-path [root]
  {:pre [(vector? root)]}
  (conj root ::dynamic-buttons))

(defn- total-clicks-path [root]
  {:pre [(vector? root)]}
  (conj root ::total-clicks))

;; Effects

(def effects {::gen-button-id
              (fn [{:keys [fx/root] :as m} dispatch!]
                (dispatch!
                  (assoc m :button-id (dc/create-decomponent root))))})

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-buttons
  [{:keys [fx/context fx/root button-id] :as m}]
  {:pre [root]}
  (if button-id
    (do
      (assert (vector? button-id))
      {:context (fx/swap-context context update-in (dynamic-buttons-path root) (fnil conj []) {:root button-id :clicked 0})})
    ; get an effect to generate a button id
    {::gen-button-id (select-keys m [:event/type :fx/root])})) ;;FIXME cannot automatically include :fx/root 

(defmethod handler ::less-buttons
  [{:keys [fx/context fx/root] :as m}]
  {:pre [root]}
  (let [dynamic-buttons (sub-in context (dynamic-buttons-path root))
        new-dynamic-buttons (if (seq dynamic-buttons)
                              (pop dynamic-buttons)
                              [])]
    (cond-> [[:context (fx/swap-context context assoc-in (dynamic-buttons-path root) new-dynamic-buttons)]]
      (seq dynamic-buttons)
      (concat [[:dispatch {:event/type ::dc-utils/delete-decomponent
                           :path (:root (peek dynamic-buttons))}]]))))

(defmethod handler ::button-clicked
  [{:keys [fx/context fx/root] :as m}]
  )

(defmethod handler :default
  [m]
  (println "No handler: " (:event/type m)))

;; Views


(defn dynamic-buttons [{:keys [fx/context fx/root]}]
  {:pre [root]}
  {:fx/type :v-box
   :children (mapv #(do
                      {:fx/type button/view
                       :fx/root %})
                   (sub-in context (dynamic-buttons-path root)))})

(defn summarize-buttons [{:keys [fx/context fx/root]}]
  {:pre [root]}
  (let [sum (fx/sub context sub-in (total-clicks-path root))]
    {:fx/type :label
     :text (str "Sum: " sum)}))

(defn create-button [{:keys [fx/root id]}]
  {:fx/type button/view
   :fx/relative-root id
   :on-action {:event/type ::button-clicked
               :fx/root root
               :button-path id}})

(defn view
  "Main view of button panes."
  [{:keys [fx/root]}]
  {:fx/type :v-box
   :spacing 10
   :children [{:fx/type :label
               :text "Buttons with static ids"}
              {:fx/type :h-box
               :children [{:fx/type create-button
                           :id ::first-button}
                          {:fx/type create-button
                           :id ::second-button}]}
              {:fx/type :label
               :text "Buttons with dynamic ids"}
              {:fx/type :h-box
               :children [{:fx/type :button
                           :on-action {:event/type ::less-buttons}
                           :text (str "Less buttons")}
                          {:fx/type :button
                           :on-action {:event/type ::more-buttons}
                           :text (str "More buttons")}]}
              {:fx/type dynamic-buttons}
              {:fx/type summarize-buttons}]})

;; Decomponent

; list of decomponent dependencies to garbage collect
(defn delete-decomponent [context root]
  (map #(vector `button/decomponent %)
       (concat (map #(dc/create-decomponent root %) [::first-button ::second-button])
               (map :root (sub-in context (dynamic-buttons-path root))))))

(def decomponent
  {:decomponents `#{button/decomponent
                    dc-utils/decomponent}
   :effects effects
   :event-handler-map (dissoc (methods handler) :default)})
