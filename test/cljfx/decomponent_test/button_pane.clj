(ns cljfx.decomponent-test.button-pane
  (:require [cljfx.api :as fx]
            [cljfx.decomponent-test.button :as button]))

;; Note:
;; Nested decomponents may result in very long paths.
;; Idea: flatten trees into a :dc/decomponents map at
;; the top-level of the tree, which is a (Map Path DecomponentState).

;; Initial State

; needs to be updated with root path
(defn init-state [] {::dynamic-ids []})

;; Decomponent paths

(defn- sub-in [context ks]
  {:pre [((every-pred vector? seq) ks)]}
  (get-in (fx/sub context (first ks)) (rest ks)))

(defn button-path-element [id]
  [::buttons id])

(defn button-path [path id]
  {:pre [(vector? path)]}
  (conj path (button-path-element id)))

(defn dynamic-ids-path [path]
  {:pre [(vector? path)]}
  (conj path ::dynamic-ids))

;; Effects

(def effects {::gen-button-id
              (fn [m dispatch!]
                (let [the-effect (gensym :button)]
                  (dispatch!
                    (assoc m :button-id the-effect))))})

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-buttons
  [{:keys [fx/context fx/path button-id] :as m}]
  {:pre [path]}
  (if button-id
    {:context (fx/swap-context context update-in (dynamic-ids-path path) (fnil conj []) button-id)}
    ; get an effect to generate a button id
    {::gen-button-id (select-keys m [:event/type :fx/path])})) ;;FIXME automatically include :fx/path 

(defmethod handler ::less-buttons
  [{:keys [fx/context fx/path] :as m}]
  {:pre [path]}
  {:context (fx/swap-context context update-in (dynamic-ids-path path)
                             #(or (when (seq %) (pop %)) []))})

(defmethod handler :default
  [m]
  (println "No handler: " (:event/type m)))

;; Views

(defn all-button-paths [context path]
  (mapv #(button-path path %)
        (concat [::first-button ::second-button]
                (sub-in context (dynamic-ids-path path)))))

(defn sum-clicks [context path]
  (reduce #(+ %1 (fx/sub context button/get-clicked %2))
          0
          (fx/sub context all-button-paths path)))

(defn dynamic-buttons [{:keys [fx/context fx/path]}]
  {:fx/type :v-box
   :children (mapv #(do
                      {:fx/type button/button-with-state
                       :fx/extend-path (button-path-element %)})
                   (sub-in context (dynamic-ids-path path)))})

(defn summarize-buttons [{:keys [fx/context fx/path]}]
  {:pre [path]}
  (let [sum (fx/sub context sum-clicks path)]
    {:fx/type :label
     :text (str "Sum: " sum)}))

(defn view [_]
  {:fx/type :v-box
   :spacing 10
   :children [{:fx/type :label
               :text "Buttons with static ids"}
              {:fx/type :h-box
               :children [{:fx/type button/button-with-state
                           :fx/extend-path (button-path-element ::first-button)}
                          {:fx/type button/button-with-state
                           :fx/extend-path (button-path-element ::second-button)}]}
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
  {:init-state (init-state)
   :decomponents `#{button/decomponent}
   :effects effects
   :event-handler-map (dissoc (methods handler) :default)})
