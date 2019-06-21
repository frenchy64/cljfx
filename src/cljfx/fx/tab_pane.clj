(ns cljfx.fx.tab-pane
  "Part of a public API"
  (:require [cljfx.composite :as composite]
            [cljfx.lifecycle :as lifecycle]
            [cljfx.coerce :as coerce]
            [cljfx.mutator :as mutator]
            [cljfx.fx.control :as fx.control]
            [cljfx.prop :as prop]
            [cljfx.mutator :as mutator])
  (:import [javafx.scene.control Tab TabPane TabPane$TabClosingPolicy TabPane$TabDragPolicy]
           [javafx.collections ObservableList]
           [javafx.geometry Side]
           [javafx.scene AccessibleRole]))

(set! *warn-on-reflection* true)

(defn sync-observable-list [^ObservableList l coll]
  (prn "sync-observable-list")
  (let [size (.size l)
        ;; update the first (count coll) elements in ObservableList
        coll-size (loop [idx 0
                         [e & es :as e-all] coll]
                    (if (seq e-all)
                      (do
                        (if (< idx size)
                          (when (not (identical? e (.get l idx)))
                            (prn "write")
                            (.set l idx e))
                          (do
                            (prn "add")
                            (.add l idx e)))
                        (recur (inc idx) es))
                      idx))]
    ;; delete tail past coll-size
    (when (< coll-size size)
      (prn "remove")
      (.remove l coll-size size))))

(defn tab-observable-list [get-list-fn]
  (let [set-all! #(sync-observable-list (get-list-fn %1) %2)]
    (with-meta
      [::tab-observable-list get-list-fn]
      {`mutator/assign! (fn [_ instance coerce value]
                          (set-all! instance (coerce value)))
       `mutator/replace! (fn [_ instance coerce old-value new-value]
                           (when-not (= old-value new-value)
                             (set-all! instance (coerce new-value))))
       `mutator/retract! (fn [_ instance _ _]
                           (set-all! instance []))})))

(def props
  (merge
    fx.control/props
    (composite/props TabPane
      ;; overrides
      :style-class [:list lifecycle/scalar :coerce coerce/style-class :default "tab-pane"]
      :accessible-role [:setter lifecycle/scalar :coerce (coerce/enum AccessibleRole)
                        :default :tab-pane]
      ;; definitions
      :rotate-graphic [:setter lifecycle/scalar :default false]
      :side [:setter lifecycle/scalar :coerce (coerce/enum Side) :default :top]
      :tab-closing-policy [:setter lifecycle/scalar
                           :coerce (coerce/enum TabPane$TabClosingPolicy)
                           :default :selected-tab]
      :tab-drag-policy [:setter lifecycle/scalar
                        :coerce (coerce/enum TabPane$TabDragPolicy) :default :fixed]
      :tab-max-height [:setter lifecycle/scalar :coerce double :default Double/MAX_VALUE]
      :tab-max-width [:setter lifecycle/scalar :coerce double :default Double/MAX_VALUE]
      :tab-min-height [:setter lifecycle/scalar :coerce double :default 0.0]
      :tab-min-width [:setter lifecycle/scalar :coerce double :default 0.0]
      :tabs (prop/make (tab-observable-list #(.getTabs ^TabPane %))
                       lifecycle/dynamics)
      :on-tabs-changed (prop/make (mutator/list-change-listener #(.getTabs ^TabPane %))
                                  lifecycle/list-change-listener))))

(def lifecycle
  (composite/describe TabPane
    :ctor []
    :props props))
