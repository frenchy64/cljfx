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

(defn- create-composite-component [this desc opts]
  (let [props-desc (desc->props-desc desc)
        props-config (:props this)
        props (create-props props-desc props-config opts)
        args (:args this)
        instance (apply (:ctor this) (map #(prop/coerce (props-config %) (props %)) args))
        arg-set (set args)
        sorted-props (if-let [prop-order (:prop-order this)]
                       (sort-by #(get prop-order (key %) 0)
                                props)
                       props)]
    (doseq [e sorted-props
            :let [k (key e)
                  v (val e)]
            :when (not (contains? arg-set k))]
      (prop/assign! (get props-config k) instance v))
    (with-meta {:props props :instance instance}
               {`component/instance :instance})))

;; TODO microbenchmark
(defn- combined-keys [m1 m2]
  (keys
    (if (< (count m2) (count m1))
      (into (or m1 {}) m2)
      (into (or m2 {}) m1))))

(defn- advance-composite-component [this component desc opts]
  (let [props-desc (desc->props-desc desc)
        props-config (:props this)
        instance (component/instance component)]
    (-> component
        (update
          :props
          (fn [props]
            (let [;; hmm, unclear whether to use a lazy sequence+distinct
                  ;; or an eager op
                  prop-keys (combined-keys props props-desc)
                  sorted-prop-keys (if-let [prop-order (:prop-order this)]
                                     (sort-by #(get prop-order % 0) prop-keys)
                                     prop-keys)]
              ;; start with {} instead of props so we don't need to dissoc on delete
              (into {}
                    (map (fn [k]
                           (let [old-e (find props k)
                                 new-e (find props-desc k)]
                             (if (some? old-e)
                               (if (some? new-e)
                                 ;; update
                                 (let [old-component (val old-e)
                                       desc (val new-e)
                                       prop-config (get props-config k)
                                       new-component (lifecycle/advance (prop/lifecycle prop-config)
                                                                        old-component
                                                                        desc
                                                                        opts)]
                                   (prop/replace! prop-config instance old-component new-component)
                                   [k new-component])
                                 ;; delete
                                 (let [prop-config (get props-config k)]
                                   (prop/retract! prop-config instance (val old-e))
                                   (lifecycle/delete (prop/lifecycle prop-config) (val old-e) opts)
                                   nil))
                               ;; create
                               ;; Note: new-e is non-nil here because (nil? old-e)
                               (let [prop-config (get props-config k)
                                     component (lifecycle/create (prop/lifecycle prop-config)
                                                                 (val new-e)
                                                                 opts)]
                                 (prop/assign! prop-config instance component)
                                 [k component])))))
                    sorted-prop-keys)))))))

(defn- delete-composite-component [this component opts]
  (let [props-config (:props this)]
    (doseq [e (:props component)]
      (lifecycle/delete (prop/lifecycle (get props-config (key e))) (val e) opts))))

(defn lifecycle [m]
  (with-meta
    m
    {`lifecycle/create create-composite-component
     `lifecycle/advance advance-composite-component
     `lifecycle/delete delete-composite-component}))

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
    `(fn ~fn-name [~instance-sym]
       (~prop-expr ~instance-sym))))

(defmacro props [type-expr & kvs]
  (into {}
        (map (fn [[k v :as e]]
               (assert (= 2 (count e)) "Uneven arguments to props")
               [k
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
                    prop)
                  v)]))
        (partition-all 2 kvs)))

(defmacro describe [type-expr & kvs]
  (let [kv-map (apply hash-map kvs)]
    `(lifecycle
       ~(into {:props (let [props (:props kv-map)]
                        (if (map? props)
                          `(props
                             ~type-expr
                             ~@(mapcat identity props))
                          props))}
              (map (fn [[k v :as e]]
                     (case k
                       :ctor
                       (let [args (map #(-> % name gensym) v)
                             ctor-sym (symbol (str type-expr "."))]
                         {:ctor `(fn [~@args]
                                   (~ctor-sym ~@args))
                          :args v})
                       e)))
              (dissoc kv-map :props)))))
