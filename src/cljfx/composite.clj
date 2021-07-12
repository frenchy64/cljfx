(ns cljfx.composite
  "Part of a public API

  [[lifecycle]] should be treated as a Lifecycle protocol implementation only, it's
  internals are subject to change"
  (:require [cljfx.component :as component]
            [cljfx.lifecycle :as lifecycle]
            [cljfx.mutator :as mutator]
            [cljfx.prop :as prop]
            [clojure.string :as str])
  (:import [java.util Locale]))

(defn- desc->props-desc [desc]
  (dissoc desc :fx/type))

(defn- create-props [props-desc props-config opts]
  (reduce-kv
    (fn [acc k v]
      (let [prop-config (get props-config k)]
        (when-not prop-config
          (throw (ex-info (str "No such prop: " (pr-str k)) {:prop k})))
        (assoc acc k (lifecycle/create (prop/lifecycle prop-config) v opts))))
    props-desc
    props-desc))

(defn- sort-plan [plan prop-order cmp]
  (cond->> plan
    prop-order (sort-by #(get prop-order (:k % 0) 0) cmp)))

(defn lifecycle [{props-config :props :keys [args ctor prop-order]}]
  (reify
    lifecycle/Lifecycle
    (lifecycle/create [_ desc opts]
      (let [props-desc (desc->props-desc desc)
            props (create-props props-desc props-config opts)
            instance (apply ctor (map #(prop/coerce (props-config %) (props %)) args))
            arg-set (set args)
            _ (-> (sequence
                    (remove (comp arg-set :k))
                    (lifecycle/create-prop-map-plan props-desc props-config))
                  (sort-plan prop-order <)
                  (lifecycle/execute-prop-plan! instance opts))]
        (with-meta {:props props :instance instance}
                   {`component/instance :instance})))
    (lifecycle/advance [_ component desc opts]
      (let [props-desc (desc->props-desc desc)
            instance (component/instance component)]
        (-> component
            (update :props #(-> %
                                (lifecycle/advance-prop-map-plan props-desc props-config)
                                (sort-plan prop-order <)
                                (lifecycle/execute-prop-plan! instance opts))))))
    (lifecycle/delete [_ {:keys [props] :as component} opts]
      (let [instance (component/instance component)]
        (-> props
            (lifecycle/delete-prop-map-plan props-config)
            (sort-plan prop-order >)
            (lifecycle/execute-prop-plan! instance opts)))
      nil)))

(defn- capitalize [^String s]
  (if (< (count s) 2)
    (.toUpperCase s Locale/ROOT)
    (str (.toUpperCase (subs s 0 1) Locale/ROOT)
         (.toLowerCase (subs s 1) Locale/ROOT))))

(defmacro setter [type-expr kw]
  (let [instance-sym (with-meta (gensym "instance") {:tag type-expr})
        value-sym (gensym "value")
        setter-expr (symbol (apply str ".set" (map capitalize (-> kw
                                                                  name
                                                                  (str/split #"-")))))
        fn-name (symbol (str "set-" (name kw)))]
    `(fn ~fn-name [~instance-sym ~value-sym]
       (~setter-expr ~instance-sym ~value-sym))))

(defmacro observable-list [type-expr kw]
  (let [instance-sym (with-meta (gensym "instance") {:tag type-expr})
        getter-expr (symbol (apply str ".get" (map capitalize (-> kw
                                                                  name
                                                                  (str/split #"-")))))
        fn-name (symbol (str "get-" (name kw)))]
    `(fn ~fn-name [~instance-sym]
       (~getter-expr ~instance-sym))))

(defmacro observable-map [type-expr kw]
  (let [instance-sym (with-meta (gensym "instance") {:tag type-expr})
        getter-expr (symbol (apply str ".get" (map capitalize (-> kw
                                                                  name
                                                                  (str/split #"-")))))
        fn-name (symbol (str "get-" (name kw)))]
    `(fn ~fn-name [~instance-sym]
       (~getter-expr ~instance-sym))))

(defmacro property-change-listener [type-expr kw]
  (let [instance-sym (with-meta (gensym "instance") {:tag type-expr})
        property-name (second (re-matches #"on-(.+)-changed" (name kw)))
        prop-parts (conj (str/split property-name #"-") "property")
        prop-expr (symbol (str "."
                               (first prop-parts)
                               (apply str (map capitalize (rest prop-parts)))))
        fn-name (symbol (str/join "-" prop-parts))]
    [prop-parts prop-expr fn-name]
    `(fn ~fn-name [~instance-sym]
       (~prop-expr ~instance-sym))))

(defmacro props [type-expr & kvs]
  `(hash-map
     ~@(->> kvs
            (partition 2)
            (mapcat
              (fn [[k v]]
                (if (vector? v)
                  (let [[mutator & args] v
                        prop `(prop/make
                                ~(case mutator
                                   :setter
                                   `(mutator/setter (setter ~type-expr ~k))

                                   :list
                                   `(mutator/observable-list
                                      (observable-list ~type-expr ~k))

                                   :map
                                   `(mutator/observable-map
                                      (observable-map ~type-expr ~k))

                                   :property-change-listener
                                   `(mutator/property-change-listener
                                      (property-change-listener ~type-expr ~k))
                                   mutator)
                                ~@args)]
                    [k prop])
                  [k v]))))))

(defmacro describe [type-expr & kvs]
  (let [kv-map (apply hash-map kvs)]
    `(lifecycle
       (hash-map ~@(mapcat
                     (fn [[k v]]
                       (case k
                         :ctor
                         (let [args (map #(-> % name gensym) v)
                               ctor-sym (symbol (str type-expr "."))]
                           `[:ctor (fn [~@args]
                                     (~ctor-sym ~@args))
                             :args ~v])

                         :props []

                         [k v]))
                     kv-map)
                 :props ~(let [props (:props kv-map)]
                           (if (map? props)
                             `(props
                                ~type-expr
                                ~@(mapcat identity props))
                             props))))))
