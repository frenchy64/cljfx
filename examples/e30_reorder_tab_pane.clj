(ns e30-reorder-tab-pane
  (:require [cljfx.api :as fx]
            [cljfx.ext.tab-pane :as ext.tab-pane])
  (:import (javafx.scene.control Tab TabPane)
           (java.util UUID)))

(set! *warn-on-reflection* true)

;; A reorderable :tab-pane where changes are synced up to the current context.
;; Uses immutable context style (co-effects/effects) and async events.
;;
;; Context structure:
;;  :current-tab-id is the currently focused tab (or nil)
;;  :tab-id-order is a vector representing the current order of tabs (by id)
;;  :tabs is a map from tab id (string) to tab, which are of the form
;;    {:tab-id String
;;     :title String
;;     :message String}

(def *context
  (atom
    (fx/create-context
      {:current-tab-id nil
       :tab-id-order []
       :tabs {}})))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;


(defn ^String short-id [^String id]
  (.substring id 0 3))

;;;;;;;;;;;
;; Subs  ;;
;;;;;;;;;;;

(defn all-tabs-by-id [context]
  {:post [(map? %)]}
  (fx/sub context :tabs))

(defn tab-by-id [context tab-id]
  {:pre [(string? tab-id)]
   :post [((some-fn nil? map) %)]}
  (-> (all-tabs-by-id context)
      (get tab-id)))

(defn tab-id-order [context]
  {:post [(vector? %)]}
  (fx/sub context :tab-id-order))

(defn current-tab-id [context]
  {:post [((some-fn nil? string?) %)]}
  (fx/sub context :current-tab-id))

(defn all-tabs
  "Returns a vector of all tabs in order of display"
  [context]
  {:post [(vector? %)]}
  (let [tab-id-order (tab-id-order context)
        tmap (all-tabs-by-id context)]
    (into []
          (mapcat (fn [tab-id]
                    (let [tab (get tmap tab-id)]
                      (when-not tab
                        (prn "Warning: Orphan tab in tab-id-order"
                             tab-id
                             tmap))
                      (some-> tab vector))))
          tab-id-order)))

(defn focused-tab [context]
  {:post [((some-fn nil? map?) %)]}
  (some->> (current-tab-id context) 
           (tab-by-id context)))

(defn focused-tab-position
  ([context] (focused-tab-position context (all-tabs context)))
  ([context tabs]
   {:pre [(vector? tabs)]
    :post [((some-fn nil?
                     (every-pred int?
                                 (complement neg?)))
            %)]}
   (let [focused-tab (focused-tab context)]
     (->> tabs
          (map-indexed vector)
          (keep (fn [[idx m]]
                  {:pre [(int? idx)
                         (map? m)]
                   :post [((some-fn nil? int?) %)]}
                  (when (= (:tab-id m) (:tab-id focused-tab))
                    idx)))
          first))))

;;;;;;;;;;;;;
;; Events  ;;
;;;;;;;;;;;;;

(defmulti event-handler :event/type)

(defmethod event-handler :default [m]
  (println "WARNING: No handler for: " (pr-str (:event/type m))))

(defmethod event-handler ::changed-tab-pane-item
  [{^Tab tab :fx/event, :keys [fx/context]}]
  (when tab
    (prn "::changed-tab-pane-item" (mapv (comp short-id #(.getId ^Tab %)) (-> tab .getTabPane .getTabs)))
    {:context (fx/swap-context context assoc :current-tab-id (.getId tab))}))

(defmethod event-handler ::create-tab
  [{:keys [fx/context]}]
  (prn "::create-tab")
  (let [tab-id (str (UUID/randomUUID))
        tab {:tab-id tab-id
             :title (short-id tab-id)
             :message tab-id}]
    {:context (fx/swap-context
                context
                #(-> %
                     (assoc-in [:tabs tab-id] tab)
                     (update :tab-id-order conj tab-id)
                     ; becomes the new focus if none already
                     (update :current-tab-id
                             (fn [current-tab-id]
                               (or current-tab-id tab-id)))))}))

(defmethod event-handler ::close-tab
  [{:keys [fx/context tab-id]}]
  {:pre [(string? tab-id)]}
  (let [tab (tab-by-id context tab-id)
        tab-order (tab-id-order context)]
    (prn "::close-tab")
    {:context (fx/swap-context
                context
                (fn [state]
                  (-> state
                     (update :tabs dissoc tab-id)
                     (update :tab-id-order #(filterv (complement #{tab-id}) %))
                     ((fn [{:keys [tab-id-order current-tab-id] :as state}]
                        (cond-> state
                          ; if closed tab is currently in focus, refocus
                          (= current-tab-id tab-id)
                          (assoc :current-tab-id (first tab-id-order))))))))
     ;; release resources held by tab in an effect
     ;; :dispatch ... tab ...
     }))

; triggered when tabs are rearranged
(defmethod event-handler ::tabs-changed
  [{tabs :fx/event :keys [fx/context]}]
  (let [new-order (mapv #(.getId ^Tab %) tabs)]
    (prn "::tabs-changed" (mapv short-id new-order))
    {:context (fx/swap-context context assoc :tab-id-order new-order)}))

; a single tab changed, doesn't seem very useful...
(defmethod event-handler ::on-selection-changed
  [{:keys [fx/context tab-id]}]
  (prn "::on-selection-changed" 
       (short-id tab-id)))

(defmethod event-handler ::sort-tabs
  [{:keys [fx/context]}]
  (let [new-order (->> (tab-id-order context) sort vec)]
    {:context (fx/swap-context context assoc :tab-id-order new-order)}))

(defmethod event-handler ::rerender [_] {:rerender nil})

;;;;;;;;;;;
;; Views ;;
;;;;;;;;;;;

(defn display-tab [{:keys [fx/context tab-id]}]
  {:pre [(string? tab-id)]}
  (let [{:keys [title message]} (get (all-tabs-by-id context) tab-id)
        _ (assert (string? message))]
    {:fx/type :tab
     :on-selection-changed {:event/type ::on-selection-changed
                            :tab-id tab-id}
     :on-closed {:event/type ::close-tab
                 :fx/sync true
                 :tab-id tab-id}
     :id tab-id
     :text title
     :closable true
     :content {:fx/type :label
               :text message}}))


(defn tab-pane [{:keys [fx/context]}]
  (let [tabs (->> (all-tabs context)
                  (mapv (fn [tab]
                          {:pre [(map? tab)]
                           :post [(string? (:fx/key %))]}
                          (-> tab
                              (assoc :fx/type display-tab)
                              (#(assoc % :fx/key (:tab-id %)))))))
        focused-position (focused-tab-position context tabs)]
    (prn "ntabs" (count tabs))
    (prn "tabs rendered order" (mapv (comp short-id :fx/key) tabs))
    (prn "focused-position" focused-position)
    {:fx/type ext.tab-pane/with-selection-props
     :props {:on-selected-item-changed {:event/type ::changed-tab-pane-item
                                        :fx/sync true}
             :selected-index (or focused-position -1)
             }
     :desc {:fx/type :tab-pane
            :tab-drag-policy :reorder
            :tab-closing-policy :all-tabs
            :on-tabs-changed {:event/type ::tabs-changed
                              :fx/sync true}
            ;:style {:-fx-open-tab-animation "NONE" :-fx-close-tab-animation "NONE"}
            :tabs tabs}}))

(defn root [{:keys [fx/context] :as m}]
  (prn "root")
  (let [tab-id-order (tab-id-order context)
        current-tab-id (current-tab-id context)
        focused-position (focused-tab-position context)]
    {:fx/type :stage
     :showing true
     :width 960
     :height 540
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :padding 20
                    :spacing 20
                    :children [{:fx/type :h-box
                                :spacing 20
                                :children
                                [{:fx/type :button
                                  :text "Create tab..."
                                  :on-action {:event/type ::create-tab
                                              :fx/sync true}}
                                 {:fx/type :button
                                  :text "Sort tabs..."
                                  :on-action {:event/type ::sort-tabs
                                              :fx/sync true}}
                                 {:fx/type :button
                                  :text "Rerender..."
                                  :on-action {:event/type ::rerender}}]}
                               {:fx/type :label
                                :text (str "Current tab id: "
                                           (pr-str current-tab-id))}
                               {:fx/type :label
                                :text (str "Focused position: "
                                           (pr-str focused-position))}
                               {:fx/type :label
                                :text (str "Current tab order: "
                                           (binding [*print-length* nil]
                                             (pr-str (mapv short-id tab-id-order))))}
                               {:fx/type tab-pane}]}}}))

;;;;;;;;;;;;;;
;; Main app ;;
;;;;;;;;;;;;;;

(declare app)

(defn rerender 
  ([] (rerender (:renderer app)))
  ([app]
   ((:renderer app))
   :ok))

(def app
  (let [this-app (atom nil)
        _ (reset! this-app
                  (fx/create-app
                    *context
                    :event-handler event-handler
                    ; don't {:fx/sync true} this effect, hangs app
                    :effects {:rerender (fn [& args] (rerender @this-app))}
                    :desc-fn (fn [_]
                               {:fx/type root})))]
    @this-app))


(comment
  (rerender)
  (fx/instance (fx/create-component {:fx/type :label}))
  (fx/on-fx-thread
    (fx/instance
      (fx/create-component
        {:fx/type :stage
         :showing true
         :scene {:fx/type :scene
                 :root {:fx/type :tab-pane}}})))

   (fx/on-fx-thread
     (fx/create-component
       {:fx/type fx/ext-many
        :desc [{:fx/type :stage
                :showing true}
               {:fx/type :stage
                :showing true}]}))
  )
