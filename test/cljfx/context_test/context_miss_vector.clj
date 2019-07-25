(ns cljfx.context-test.context-miss-vector
  (:require [cljfx.api :as fx]))

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-buttons
  [{:keys [fx/context id] :as m}]
  {:context (fx/swap-context context update ::ids (fnil conj []) (gensym :id))})

(defmethod handler ::less-buttons
  [{:keys [fx/context] :as m}]
  {:context (fx/swap-context context update ::ids
                             #(or (when (seq %) (pop %)) []))})

(defmethod handler ::clicked
  [{:keys [fx/context id] :as m}]
  {:context (fx/swap-context context update-in [::clicked id] (fnil inc 0))})

;; Views

(defn buttons [{:keys [fx/context]}]
  {:fx/type :scroll-pane
   :fit-to-width true
   :fit-to-height true
   :content
   {:fx/type :h-box
    :children (mapv #(do
                       {:fx/type :button
                        :text (str "x" (get (fx/sub context ::clicked) % 0))
                        :on-action {:event/type ::clicked
                                    :id %}})
                    (fx/sub context ::ids))}})

(defn sum-buttons [context]
  (reduce #(+ %1 (get (fx/sub context ::clicked) %2 0))
          0
          (fx/sub context ::ids)))

(defn view [{:keys [fx/context] :as m}]
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
                                :on-action {:event/type ::less-buttons}
                                :text (str "Less button panes")}
                               {:fx/type :button
                                :on-action {:event/type ::more-buttons}
                                :text (str "More button panes")}]}
                   {:fx/type :label
                    :text (str "Sum: " (fx/sub context sum-buttons))}
                   {:fx/type buttons}]}}})

;; Main app

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app)))

(def *context
  (atom (fx/create-context {})))

(def app
  (fx/create-app *context
    :event-handler handler
    :desc-fn (fn [_]
               {:fx/type view})))
