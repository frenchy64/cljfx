(ns cljfx.composite-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [cljfx.component :as component]
            [cljfx.composite :as composite]
            [cljfx.lifecycle :as lifecycle]
            [cljfx.mutator :as mutator]
            [cljfx.prop :as prop]))

(deftest composite-lifecycle-test
  (let [state (atom {:history []
                     :obj 0
                     :props {}})
        grab-history (fn []
                       (let [[{:keys [history]}]
                             (swap-vals! state assoc :history [])]
                         history))
        mk-prop (fn [pkw]
                  (prop/make (mutator/setter
                               (fn [_ v]
                                 (swap! state
                                        #(-> %
                                             (assoc-in [:props pkw] v)
                                             (update :history conj
                                                     {:op :set-prop
                                                      :prop pkw
                                                      :v v})))))
                             lifecycle/scalar))
        ;; :b first, :c last
        prop-order {:b -1
                    :c 1}
        props-config (into {}
                           (map (juxt identity mk-prop))
                           [:a :b :c])
        lifecycle (composite/lifecycle
                    {:ctor (fn [& args]
                             (-> (swap! state
                                        #(-> %
                                             (update :obj inc)
                                             (update :history conj
                                                     {:op :ctor
                                                      :args (vec args)})))
                                 :obj))
                     :args [:a]
                     :props props-config
                     :prop-order prop-order})
        ;; Note: `component` shadowing to prevent mistakes
        component (lifecycle/create
                    lifecycle
                    {:fx/type lifecycle
                     :a 42
                     :b 24}
                    nil)
        _ (fact (grab-history)
                => [{:op :ctor, :args [42]}
                    {:op :set-prop, :prop :b, :v 24}])
        _ (fact (component/instance component)
                => 1)

        component (lifecycle/advance
                    lifecycle
                    component
                    {:fx/type lifecycle
                     :a 2}
                    nil)
        _ (fact (grab-history)
                => [{:op :set-prop, :prop :b, :v nil}
                    {:op :set-prop, :prop :a, :v 2}])
        _ (fact (component/instance component)
                => 1)

        component (lifecycle/advance
                    lifecycle
                    component
                    {:fx/type lifecycle
                     :a 1
                     :b 2
                     :c 3}
                    nil)
        _ (fact (grab-history)
                => [{:op :set-prop, :prop :b, :v 2}
                    {:op :set-prop, :prop :a, :v 1}
                    {:op :set-prop, :prop :c, :v 3}])
        _ (fact (component/instance component)
                => 1)

        component (lifecycle/advance
                    lifecycle
                    component
                    {:fx/type lifecycle
                     :a 1
                     :b 2
                     :c 3}
                    nil)
        _ (fact (grab-history)
                => [])
        _ (fact (component/instance component)
                => 1)

        component (lifecycle/delete
                    lifecycle
                    component
                    nil)
        _ (fact (grab-history)
                => [{:op :set-prop, :prop :c, :v nil}
                    {:op :set-prop, :prop :a, :v nil}
                    {:op :set-prop, :prop :b, :v nil}])
        _ (fact (component/instance component)
                => nil)
        ]))
