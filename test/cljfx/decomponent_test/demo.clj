(ns cljfx.decomponent-test.demo
  (:require [cljfx.api :as fx]
            [cljfx.decomponent-test.button-pane :as button-pane]
            [cljfx.decomponent-test.dc-utils :as dc-utils]))

(defn button-pane-path [root id]
  {:pre [(vector? root)]}
  (conj root [::button-pane id]))

;; Event handler

(defmulti handler :event/type)

(defmethod handler ::more-button-panes
  [{:keys [fx/context fx/root] :as m}]
  {:context (fx/swap-context context update ::dynamic-ids (fnil conj [])
                             (button-pane-path root (gensym :pane)))})

(defmethod handler ::less-button-panes
  [{:keys [fx/context] :as m}]
  (let [dynamic-ids (fx/sub context ::dynamic-ids)
        new-dynamic-ids (if (seq dynamic-ids)
                          (pop dynamic-ids)
                          [])]
    (cond-> [[:context (fx/swap-context context assoc ::dynamic-ids new-dynamic-ids)]]
      (seq dynamic-ids)
      (concat [[:dispatch {:event/type ::dc-utils/delete-decomponent
                           :path (peek dynamic-ids)}]]))))

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
                        :fx/root %})
                    (fx/sub context ::dynamic-ids))}})

(defn sum-clicks [context]
  (reduce #(+ %1 (fx/sub context button-pane/sum-clicks %2))
          0
          (fx/sub context ::dynamic-ids)))

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
                                :on-action {:event/type ::less-button-panes}
                                :text (str "Less button panes")}
                               {:fx/type :button
                                :on-action {:event/type ::more-button-panes}
                                :text (str "More button panes")}
                               {:fx/type :button
                                :on-action {:event/type ::flip-layout}
                                :text (str "Flip layout")}]}
                   {:fx/type :label
                    :text (str "Grand total clicks: " (fx/sub context sum-clicks))}
                   {:fx/type dynamic-button-panes}]}}})

;; Main app

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app)))

(def *context
  (atom (fx/create-context {})))

(def app
  (fx/create-app *context
    :decomponents `#{button-pane/decomponent
                     dc-utils/decomponent}
    :decomponent-root ::decomponents
    :event-handler handler
    :desc-fn (fn [_]
               {:fx/type view})))
