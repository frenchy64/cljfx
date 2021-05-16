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

(defn- advance-composite-component [this component desc opts]
  (let [props-desc (desc->props-desc desc)
        props-config (:props this)
        instance (component/instance component)]
    (-> component
        (update
          :props
          (fn [props]
            ;; Notes
            ;; - this might save some work in the sorted case, but does it
            ;;   make the unsorted (more common) case slower?
            ;;   - just using `sequence` + distinct might be faster there
            (let [; sort and group props
                  ; entries are either:
                  ; - k=>[old-e new-e] for updates
                  ; - k=>[old-e] for deletions
                  ; - k=>[nil new-e] for creations
                  sorted-old+new-props (let [;; start with sorted map if needed
                                             empty-sorted-props
                                             (if-let [prop-order (:prop-order this)]
                                               ;; TODO unit test sorted case
                                               (sorted-map-by #(compare (get prop-order %1 0)
                                                                        (get prop-order %2 0)))
                                               {})
                                             ;; add old props as {k [old-e] ...}
                                             sorted-old-props
                                             (into empty-sorted-props
                                                   (map (juxt key vector))
                                                   props)]
                                         ;; add new props as {k [(or nil old-e) new-e] ...}
                                         (reduce (fn [acc new-e]
                                                   (let [k (key new-e)]
                                                     (assoc acc k
                                                            (if-let [old-e-vec (get acc k)]
                                                              ;; [old-e new-e]
                                                              (conj old-e-vec new-e)
                                                              [nil new-e]))))
                                                 sorted-old-props
                                                 props-desc))]
              (into {}
                    (map (fn [e]
                           (let [k (key e)
                                 group (val e)
                                 old-e (nth group 0)
                                 new-e (nth group 1 nil)]
                             (if (some? old-e)
                               (if (some? new-e)
                                 ;; replace
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
                                 (let [old-component (val old-e)
                                       prop-config (get props-config k)]
                                   (prop/retract! prop-config instance old-component)
                                   (lifecycle/delete (prop/lifecycle prop-config) old-component opts)
                                   nil))
                               ;; create
                               (let [prop-config (get props-config k)
                                     component (lifecycle/create (prop/lifecycle prop-config)
                                                                 ;; note: new-e exists because old-e is nil
                                                                 (val new-e)
                                                                 opts)]
                                 (prop/assign! prop-config instance component)
                                 [k component])))))
                    sorted-old+new-props)))))))

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
