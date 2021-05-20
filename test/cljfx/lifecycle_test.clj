(ns cljfx.lifecycle-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [clojure.data]
            [cljfx.context :as context]
            [cljfx.prop :as prop]
            [cljfx.mutator :as mutator]
            [cljfx.lifecycle :as lifecycle]
            [cljfx.component :as component]
            [cljfx.api :as fx])
  (:import [javafx.scene.control Label]))

(deftest env-test
  (let [label (fn [{:keys [a b]}]
                {:fx/type :label
                 :text (str :a " " a ", " :b " " b)})
        c-1 (fx/create-component {:fx/type fx/ext-set-env
                                  :env {:a 1 :b 2}
                                  :desc {:fx/type fx/ext-get-env
                                         :env [:a :b]
                                         :desc {:fx/type label}}})
        ^Label i-1 (fx/instance c-1)
        _ (fact (.getText i-1) => ":a 1, :b 2")
        c-2 (fx/advance-component c-1 {:fx/type fx/ext-set-env
                                       :env {:a 2 :b 2}
                                       :desc {:fx/type fx/ext-get-env
                                              :env [:a :b]
                                              :desc {:fx/type label}}})
        ^Label i-2 (fx/instance c-2)
        _ (fact (.getText i-2) => ":a 2, :b 2")
        c-3 (fx/advance-component c-2 {:fx/type fx/ext-set-env
                                       :env {:x 1 :y 2}
                                       :desc {:fx/type fx/ext-get-env
                                              :env {:x :a
                                                    :y :b}
                                              :desc {:fx/type label}}})
        ^Label i-3 (fx/instance c-3)
        _ (fact (.getText i-3) => ":a 1, :b 2")
        _ (fact (= i-1 i-2 i-3) => true)]))

(def box-prop-config (prop/make (mutator/setter (fn [_ v] v)) lifecycle/dynamic))

(deftest advance-prop-map-plan-test
  (let [props-config {:a box-prop-config
                      :b box-prop-config
                      :c box-prop-config}
        _ (fact (lifecycle/advance-prop-map-plan
                  {}
                  {:a 1}
                  props-config)
                => [{:op :assign!
                     :k :a
                     :desc 1
                     :prop-config (:a props-config)}])
        _ (fact (lifecycle/advance-prop-map-plan
                  {:a 1}
                  {}
                  props-config)
                => [{:op :retract!
                     :k :a
                     :component 1
                     :prop-config (:a props-config)}])
        _ (fact (lifecycle/advance-prop-map-plan
                  {:a 1}
                  {:a 2}
                  props-config)
                => [{:op :replace!
                     :k :a
                     :component 1
                     :desc 2
                     :prop-config (:a props-config)}])]))

(deftest create-prop-map-plan-test
  (let [props-config {:a box-prop-config
                      :b box-prop-config
                      :c box-prop-config}
        _ (fact (lifecycle/create-prop-map-plan
                  {}
                  props-config)
                => [])
        _ (fact (lifecycle/create-prop-map-plan
                  {:a 1}
                  props-config)
                => [{:op :assign!
                     :k :a
                     :desc 1
                     :prop-config (:a props-config)}])]))

(deftest delete-prop-map-plan-test
  (let [props-config {:a box-prop-config
                      :b box-prop-config
                      :c box-prop-config}
        _ (fact (lifecycle/delete-prop-map-plan
                  {}
                  props-config)
                => [])
        _ (fact (lifecycle/delete-prop-map-plan
                  {:a 1}
                  props-config)
                => [{:op :retract!
                     :k :a
                     :component 1
                     :prop-config (:a props-config)}])]))

(defn mk-state [init]
  (let [state (atom (assoc init :history []))
        grab-history (fn []
                       (let [[{:keys [history]}]
                             (swap-vals! state assoc :history [])]
                         history))]
    {:state state
     :grab-history grab-history}))

(defn mk-props [pks]
  (let [{:keys [state] :as state-m} (mk-state {:props {}})
        mk-prop (fn [pkw]
                  (prop/make (mutator/setter (fn [_ v]
                                               (swap! state
                                                      #(-> %
                                                           (assoc-in [:props pkw] v)
                                                           (update :history conj
                                                                   {:op :set-prop
                                                                    :prop pkw
                                                                    :v v})))))
                             lifecycle/scalar))
        props-config (into {}
                           (map (juxt identity mk-prop))
                           pks)]
    (assoc state-m :props-config props-config)))

(deftest execute-prop-plan!-test
  (let [{:keys [state props-config grab-history]} (mk-props [:a :b :c])
        _ (swap! state assoc :props {:a 1 :c 1})

        _ (fact (lifecycle/execute-prop-plan!
                  [{:op :retract!
                    :k :a
                    :component 1
                    :prop-config (:a props-config)}
                   {:op :assign!
                    :k :b
                    :desc 42
                    :prop-config (:b props-config)}
                   {:op :replace!
                    :k :c
                    :component 1
                    :desc 24
                    :prop-config (:c props-config)}]
                  nil
                  props-config)
                => {:b 42
                    :c 24})
        _ (fact (grab-history)
                => [{:op :set-prop, :prop :a, :v nil}
                    {:op :set-prop, :prop :b, :v 42}
                    {:op :set-prop, :prop :c, :v 24}])

        ;; don't allow duplicate keys in plan
        _ (fact (lifecycle/execute-prop-plan!
                  [{:op :retract!
                    :k :a
                    :component 24
                    :prop-config (:a props-config)}
                   {:op :retract!
                    :k :a
                    :component 24
                    :prop-config (:a props-config)}]
                  nil
                  props-config)
                =throws=> AssertionError)
        ]))

(deftest wrap-extra-props-test
  (let [extra-props #{:a :b :c}
        existing-props #{:d :e}
        {:keys [state grab-history props-config]} (mk-props (concat extra-props existing-props))
        _ (swap! state assoc :obj 0)
        sort-history (fn [h]
                       (let [[extra existing] (split-with (comp extra-props :prop) h)]
                         (mapcat (partial sort-by :prop) [extra existing])))
        grab-sorted-history (comp sort-history grab-history)
        obj (atom 0)
        lifecycle (-> (with-meta
                        [::existing-props]
                        {`lifecycle/create
                         (fn [_ desc _]
                           (swap! state #(-> %
                                             (update :history into
                                                     (map (fn [[prop v]]
                                                            {:op :set-prop
                                                             :prop prop
                                                             :v v})
                                                          desc))))
                           (swap! obj inc))
                         `lifecycle/advance
                         (fn [_ _ desc opts]
                           (swap! state #(-> %
                                             (update :history into
                                                     (map (fn [[prop v]]
                                                            {:op :set-prop
                                                             :prop prop
                                                             :v v})
                                                          desc))))
                           @obj)
                         `lifecycle/delete
                         (fn [_ _ opts]
                           (swap! state #(-> %
                                             (update :history into
                                                     (map (fn [prop]
                                                            {:op :set-prop
                                                             :prop prop
                                                             :v nil})
                                                          existing-props))))
                           nil)})
                      (lifecycle/wrap-extra-props
                        (select-keys props-config extra-props)))

        component (lifecycle/create
                    lifecycle
                    {:a 1
                     :b 2
                     :c 3
                     :d 4
                     :e 5}
                    nil)
        _ (fact (grab-sorted-history)
                => [{:op :set-prop, :prop :a, :v 1}
                    {:op :set-prop, :prop :b, :v 2}
                    {:op :set-prop, :prop :c, :v 3}
                    {:op :set-prop, :prop :d, :v 4}
                    {:op :set-prop, :prop :e, :v 5}])
        _ (fact (component/instance component)
                => 1)

        ;; update just extra prop
        component (lifecycle/advance
                    lifecycle
                    component
                    {:a 1
                     :b 3
                     :c 3
                     :d 4
                     :e 5}
                    nil)
        _ (fact (grab-sorted-history)
                => [{:op :set-prop, :prop :b, :v 3}
                    {:op :set-prop, :prop :d, :v 4}
                    {:op :set-prop, :prop :e, :v 5}])
        _ (fact (component/instance component)
                => 1)

        ;; update just an existing prop
        component (lifecycle/advance
                    lifecycle
                    component
                    {:a 1
                     :b 3
                     :c 3
                     :d 2
                     :e 5}
                    nil)
        _ (fact (grab-sorted-history)
                => [{:op :set-prop, :prop :d, :v 2}
                    {:op :set-prop, :prop :e, :v 5}])
        _ (fact (component/instance component)
                => 1)

        ;; same desc but different instance -- reset all props
        _ (swap! obj inc)
        _ (fact (component/instance component)
                => 1)
        component (lifecycle/advance
                    lifecycle
                    component
                    {:a 1
                     :b 3
                     :c 3
                     :d 2
                     :e 5}
                    nil)
        _ (let [h (grab-history)
                [existing-assigns h] (split-at 2 h)
                [extra-retracts extra-assigns] (split-at 3 h)]
            ;; TODO check that this is done on the new instance
            (fact (sort-by :prop existing-assigns)
                  => [{:op :set-prop, :prop :d, :v 2}
                      {:op :set-prop, :prop :e, :v 5}])
            ;; TODO check that this is done on the old instance
            (fact (sort-by :prop extra-retracts)
                => [{:op :set-prop, :prop :a, :v nil}
                    {:op :set-prop, :prop :b, :v nil}
                    {:op :set-prop, :prop :c, :v nil}])
            ;; TODO check that this is done on the new instance
            (fact (sort-by :prop extra-assigns)
                => [{:op :set-prop, :prop :a, :v 1}
                    {:op :set-prop, :prop :b, :v 3}
                    {:op :set-prop, :prop :c, :v 3}]))
        _ (fact (component/instance component)
                => 2)

        component (lifecycle/delete
                    lifecycle
                    component
                    nil)
        _ (fact (grab-sorted-history)
                => [{:op :set-prop, :prop :a, :v nil}
                    {:op :set-prop, :prop :b, :v nil}
                    {:op :set-prop, :prop :c, :v nil}
                    {:op :set-prop, :prop :d, :v nil}
                    {:op :set-prop, :prop :e, :v nil}])
        _ (fact (component/instance component)
                => nil)
        ]
    ))

(deftest make-ext-with-props-test
  (let [{:keys [grab-history props-config]} (mk-props [:a :b :c])
        grab-sorted-history #(->> (grab-history)
                                  (sort-by :prop))
        lifecycle (lifecycle/make-ext-with-props
                    lifecycle/scalar
                    props-config)
        component (lifecycle/create
                    lifecycle
                    {:fx/type lifecycle
                     :desc 1
                     :props {:a 1
                             :b 2
                             :c 3}}
                    nil)
        _ (fact (grab-sorted-history)
                => [{:op :set-prop, :prop :a, :v 1}
                    {:op :set-prop, :prop :b, :v 2}
                    {:op :set-prop, :prop :c, :v 3}])
        _ (fact (component/instance component)
                => 1)

        component (lifecycle/advance
                    lifecycle
                    component
                    {:fx/type lifecycle
                     :desc 1
                     :props {:a 2}}
                    nil)
        _ (fact (grab-sorted-history)
                => [{:op :set-prop, :prop :a, :v 2}
                    {:op :set-prop, :prop :b, :v nil}
                    {:op :set-prop, :prop :c, :v nil}])
        _ (fact (component/instance component)
                => 1)

        ;; same desc
        component (lifecycle/advance
                    lifecycle
                    component
                    {:fx/type lifecycle
                     :desc 1
                     :props {:a 2}}
                    nil)
        _ (fact (grab-sorted-history)
                => [])
        _ (fact (component/instance component)
                => 1)

        ;; update :desc, props reset
        component (lifecycle/advance
                    lifecycle
                    component
                    {:fx/type lifecycle
                     :desc 2
                     :props {:a 2}}
                    nil)
        _ (fact (grab-sorted-history)
                => [{:op :set-prop, :prop :a, :v 2}])
        _ (fact (component/instance component)
                => 2)

        component (lifecycle/delete
                    lifecycle
                    component
                    nil)
        _ (fact (grab-sorted-history)
                ;; FIXME why doesn't this `retract!`?
                => [])
        _ (fact (component/instance component)
                => nil)
        ]))

(defn mk-logging-lifecycle []
  (let [{:keys [state] :as state-m} (mk-state {})
        logging-lifecycle
        (reify
          lifecycle/Lifecycle
          (lifecycle/create [_ desc opts]
            (swap! state #(-> %
                              (update :history conj
                                      {:op :create
                                       :desc desc
                                       :opts opts})))
            :create)
          (lifecycle/advance [_ component desc opts]
            (let [[{:keys [next-advance-instance]}]
                  (swap-vals! state #(-> %
                                         (dissoc :next-advance-instance)
                                         (update :history conj
                                                 {:op :advance
                                                  :component component
                                                  :desc desc
                                                  :opts opts})))]
              (or next-advance-instance :advance)))
          (lifecycle/delete [_ component opts]
            (swap! state #(-> %
                              (update :history conj
                                      {:op :delete
                                       :component component
                                       :opts opts})))
            :delete))]
    (assoc state-m :logging-lifecycle logging-lifecycle)))

(deftest wrap-context-desc-test
  (let [{:keys [grab-history logging-lifecycle]} (mk-logging-lifecycle)
        lifecycle (lifecycle/wrap-context-desc logging-lifecycle)

        context (context/create {:a 1 :b 2} identity)
        component (lifecycle/create
                    lifecycle
                    context
                    {::foo 1})
        _ (fact (grab-history)
                => [{:op :create
                     :desc context
                     :opts {::foo 1
                            :fx/context context}}])
        _ (fact (component/instance component)
                => :create)

        context (context/swap context update :a inc)
        component (lifecycle/advance
                    lifecycle
                    component
                    context
                    {::foo 2})
        _ (fact (grab-history)
                => [{:op :advance
                     :component :create
                     :desc context
                     :opts {::foo 2
                            :fx/context context}}])
        _ (fact (component/instance component)
                => :advance)

        component (lifecycle/delete
                    lifecycle
                    component
                    {::foo 3})
        _ (fact (grab-history)
                => [{:op :delete
                     :component :advance
                     :opts {::foo 3}}])
        _ (fact (component/instance component)
                => :delete)
        ]))

(deftest context-fn->dynamic-test
  (let [{:keys [state grab-history logging-lifecycle]} (mk-logging-lifecycle)
        lifecycle lifecycle/context-fn->dynamic

        type->lifecycle (constantly nil)
        context0 (context/create {:a 1 :b 2} identity)

        component (lifecycle/create
                    lifecycle
                    {:fx/type (fn [desc]
                                (swap! state update :history conj
                                       {:op :sub-context-fn
                                        :desc desc})
                                {:fx/type logging-lifecycle
                                 :d 4})
                     :c 3}
                    {:fx/context context0
                     :fx.opt/type->lifecycle type->lifecycle})

        _ (let [h (grab-history)
                context1 (get-in h [0 :desc :fx/context])]
            (fact h
                  => [{:op :sub-context-fn
                       :desc {:c 3
                              :fx/context context1}}
                      {:op :create
                       :desc {:fx/type logging-lifecycle
                              :d 4}
                       :opts {:fx/context context0
                              :fx.opt/type->lifecycle type->lifecycle}}]))

        context2 (context/create {:a 1 :b 2} identity)
        component (lifecycle/advance
                    lifecycle
                    component
                    {:fx/type (fn [desc]
                                (swap! state update :history conj
                                       {:op :sub-context-fn
                                        :desc desc})
                                {:fx/type logging-lifecycle
                                 :d 4})
                     :c 3}
                    {:fx/context context2
                     :fx.opt/type->lifecycle type->lifecycle})

        _ (let [h (grab-history)
                context3 (get-in h [0 :desc :fx/context])]
            (fact h
                  => [{:op :sub-context-fn
                       :desc {:c 3
                              :fx/context context3}}
                      {:op :advance
                       :component :create
                       :desc {:fx/type logging-lifecycle
                              :d 4}
                       :opts {:fx/context context2
                              :fx.opt/type->lifecycle type->lifecycle}}]))

        context4 (context/create {:a 1 :b 2} identity)
        component (lifecycle/delete
                    lifecycle
                    component
                    {:fx/context context4
                     :fx.opt/type->lifecycle type->lifecycle})
        _ (fact (grab-history)
                => [{:op :delete
                     :component :advance
                     :opts {:fx/context context4
                            :fx.opt/type->lifecycle type->lifecycle}}])
        ]))

(deftest wrap-on-instance-lifecycle-test
  (let [{:keys [state grab-history logging-lifecycle]} (mk-logging-lifecycle)
        lifecycle (lifecycle/wrap-on-instance-lifecycle logging-lifecycle)

        on-created (fn [instance]
                     (swap! state update :history conj
                            {:op :on-created
                             :instance instance}))
        on-advanced (fn [old-instance new-instance]
                      (swap! state update :history conj
                             {:op :on-advanced
                              :old-instance old-instance
                              :new-instance new-instance}))
        on-deleted (fn [instance]
                     (swap! state update :history conj
                            {:op :on-deleted
                             :instance instance}))
        component (lifecycle/create
                    lifecycle
                    {:on-created on-created
                     :desc {:a 1}}
                    {::foo 2})
        _ (fact (grab-history)
                => [{:op :create
                     :desc {:a 1}
                     :opts {::foo 2}}
                    {:op :on-created
                     :instance :create}])

        component (lifecycle/advance
                    lifecycle
                    component
                    {:on-advanced on-advanced
                     :desc {:a 2}}
                    {::foo 2})
        _ (fact (grab-history)
                => [{:op :advance
                     :component :create
                     :desc {:a 2}
                     :opts {::foo 2}}
                    {:op :on-advanced
                     :old-instance :create
                     :new-instance :advance}])

        ;; same child instance, no on-advanced call
        component (lifecycle/advance
                    lifecycle
                    component
                    {:on-advanced on-advanced
                     :desc {:a 2}}
                    {::foo 2})
        _ (fact (grab-history)
                => [{:op :advance
                     :component :advance
                     :desc {:a 2}
                     :opts {::foo 2}}])

        ;; different child instance, trigger on-advanced
        _ (swap! state assoc :next-advance-instance :advance1)
        component (lifecycle/advance
                    lifecycle
                    component
                    {:on-advanced on-advanced
                     :desc {:a 2}}
                    {::foo 2})
        _ (fact (grab-history)
                => [{:op :advance
                     :component :advance
                     :desc {:a 2}
                     :opts {::foo 2}}
                    {:op :on-advanced
                     :old-instance :advance
                     :new-instance :advance1}])

        ;; delete w/o :on-deleted
        component (lifecycle/delete
                    lifecycle
                    component
                    {::foo 2})
        _ (fact (grab-history)
                => [{:op :delete
                     :component :advance1
                     :opts {::foo 2}}])

        ;; delete with :on-deleted from create
        component (as-> (lifecycle/create
                          lifecycle
                          {:on-deleted on-deleted}
                          {::foo 2})
                    component
                    (lifecycle/delete
                      lifecycle
                      component
                      {::foo 2}))
        _ (fact (grab-history)
                => [{:op :create :desc nil :opts {::foo 2}}
                    {:op :delete :component :create :opts {::foo 2}}
                    {:op :on-deleted :instance :create}])

        ;; delete with :on-deleted from advance
        component (as-> (lifecycle/create
                          lifecycle
                          {}
                          {::foo 2})
                    component
                    (lifecycle/advance
                      lifecycle
                      component
                      {:on-deleted on-deleted}
                      {::foo 2})
                    (lifecycle/delete
                      lifecycle
                      component
                      {::foo 2}))
        _ (fact (grab-history)
                => [{:op :create :desc nil :opts {::foo 2}}
                    {:op :advance :component :create :desc nil :opts {::foo 2}}
                    {:op :delete :component :advance :opts {::foo 2}}
                    {:op :on-deleted :instance :advance}])
        ]))
