(ns cljfx.decomponent-test.button-pane
  (:require [cljfx.api :as fx]
            [cljfx.decomponent-test.button :as button]
            [cljfx.decomponent-test.dc-utils :as dc-utils]))

;; Note:
;; Nested decomponents may result in very long paths.
;; Idea: flatten trees into a :dc/decomponents map at
;; the top-level of the tree, which is a (Map Path DecomponentState).

;; Decomponent paths

(defn- sub-in [context ks]
  {:pre [((every-pred vector? seq) ks)]}
  (get-in (fx/sub context (first ks)) (rest ks)))

(defn button-path-element [id]
  [::buttons id])

(defn button-path [root id]
  {:pre [(vector? root)]}
  (conj root (button-path-element id)))

(defn dynamic-ids-path [root]
  {:pre [(vector? root)]}
  (conj root ::dynamic-ids))

;; Effects

(def effects {::gen-button-id
              (fn [{:keys [fx/root] :as m} dispatch!]
                (dispatch!
                  (assoc m :button-id (button-path root (gensym :button)))))})

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-buttons
  [{:keys [fx/context fx/root button-id] :as m}]
  {:pre [root]}
  (if button-id
    (do
      (assert (vector? button-id))
      {:context (fx/swap-context context update-in (dynamic-ids-path root) (fnil conj []) button-id)})
    ; get an effect to generate a button id
    {::gen-button-id (select-keys m [:event/type :fx/root])})) ;;FIXME cannot automatically include :fx/root 

(defmethod handler ::less-buttons
  [{:keys [fx/context fx/root] :as m}]
  {:pre [root]}
  (let [dynamic-ids (sub-in context (dynamic-ids-path root))
        new-dynamic-ids (if (seq dynamic-ids)
                          (pop dynamic-ids)
                          [])]
    (cond-> [[:context (fx/swap-context context assoc-in (dynamic-ids-path root) new-dynamic-ids)]]
      (seq dynamic-ids)
      (concat [[:dispatch {:event/type ::dc-utils/delete-decomponent
                           :path (peek dynamic-ids)}]]))))

(defmethod handler :default
  [m]
  (println "No handler: " (:event/type m)))

;; Views

(defn all-button-paths [context root]
  (mapv #(button-path root %)
        (concat [::first-button ::second-button]
                (sub-in context (dynamic-ids-path root)))))

(defn sum-clicks [context root]
  {:pre [root]}
  (reduce #(+ %1 (fx/sub context button/get-clicked %2))
          0
          (fx/sub context all-button-paths root)))

(defn dynamic-buttons [{:keys [fx/context fx/root]}]
  {:pre [root]}
  {:fx/type :v-box
   :children (mapv #(do
                      {:fx/type button/view
                       :fx/root %})
                   (sub-in context (dynamic-ids-path root)))})

(defn summarize-buttons [{:keys [fx/context fx/root]}]
  {:pre [root]}
  (let [sum (fx/sub context sum-clicks root)]
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
               :children [{:fx/type button/view
                           :fx/root (button-path root ::first-button)}
                          {:fx/type button/view
                           :fx/root (button-path root ::second-button)}]}
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

(def decomponent
  {:decomponents `#{button/decomponent
                    dc-utils/decomponent}
   :effects effects
   :event-handler-map (dissoc (methods handler) :default)})
