(ns cljfx.testfx
  (:require [cljfx.api :as fx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [org.testfx.robot Motion BaseRobot]
           [javafx.geometry Point2D Bounds]
           [javafx.scene Node Scene]
           [javafx.scene.input MouseButton KeyCode]))

(set! *warn-on-reflection* true)

(def ^:private ^Motion coerce-motion (coerce/enum Motion))

(def ^:private ^MouseButton coerce-mouse-button (coerce/enum javafx.scene.input.MouseButton))

(defn- ^Point2D coerce-point2d [p]
  (cond
    (instance? Point2D p) p
    (map? p) (Point2D. (:x p) (:y p))
    (and (vector? p)
         (= 2 (count p))) (let [[x y] p]
                            (Point2D. x y))
    :else (coerce/fail Point2D p)))

(defn- ^Bounds coerce-bounds [p]
  {:pre [(instance? Bounds p)]}
  ;TODO
  p)

(defmacro from-context [robot meth]
  (let [instance-sym (with-meta (gensym "robot") {:tag 'org.testfx.api.FxRobot})
        getter-expr (if (keyword? meth)
                      (symbol (apply str ".get" (map str/capitalize (-> meth
                                                                        name
                                                                        (str/split #"-")))))
                      meth)]
    `(let [~instance-sym ~robot]
       (-> ~instance-sym .robotContext ~getter-expr))))

(defn- normalize-exec [v]
  (if (vector? v)
    {:args (into {}
                 (map (fn [{:keys [key] :as arg}]
                        [key (dissoc arg :key :optional :one-of :only-with)]))
                 v)
     :arg-groups #{(mapv #(select-keys % [:optional :key :one-of :only-with])
                         v)}}
    v))

(defn- gen-group-branch*
  ([m meth target arg-impl-map prev group]
   {:pre [((every-pred vector?) group prev)]}
   (let [gen-group-branch* #(gen-group-branch* m meth target arg-impl-map %1 %2)]
     (if (empty? group)
       (let [impls (map (fn [p]
                          (let [k (:key p)
                                impl (get arg-impl-map k)]
                            (assert impl (str "No impl for " k))
                            impl))
                        prev)]
         (concat [meth target] impls))
       (let [{:keys [key optional one-of only-with] :as fst} (first group)
             _ (assert (empty? (dissoc fst :key :optional :one-of :only-with))
                       (str "Invalid spec keys: " (set (keys (dissoc fst :key :optional :one-of :only-with)))))
             _ (assert (map? fst))
             _ (assert (keyword? key))
             _ (assert (not (and optional one-of)))
             expected-pos (or optional one-of)
             _ (assert ((some-fn nil? set?) expected-pos))
             current-pos (count prev)
             missing-deps (when only-with
                            ; vector is disjuction, set is conjunction
                            (some #(not-empty (set/difference % (into #{} (map :key) prev)))
                                  (if (set? only-with)
                                    [only-with]
                                    only-with)))]
         (cond
           missing-deps
           `(throw (ex-info ~(str "Key provided without dependency")
                            {:key ~key
                             :dependency ~missing-deps}))

           expected-pos
           (let [throw-one-of (fn []
                                `(throw (ex-info ~(str "Must provide one of keys")
                                                 {:pos ~current-pos
                                                  :keys ~(conj (into #{}
                                                                     (comp (filter (comp #(contains? % current-pos)
                                                                                         (some-fn :optional :one-of)))
                                                                           (map :key))
                                                                     prev)
                                                               key)})))
                 throw-ambiguous (fn []
                                   `(throw (ex-info "Disallowed key combination"
                                                    {:keys ~(conj
                                                              (into #{}
                                                                    (comp (filter (comp #{one-of} :one-of))
                                                                          (map :key))
                                                                    prev)
                                                              key)})))
                 dispose #(gen-group-branch* prev (subvec group 1))]
             (if (contains? expected-pos current-pos)
               `(if (contains? ~m ~key)
                  ~(gen-group-branch* (conj prev fst) (subvec group 1))
                  ~(if (and one-of (not (some-> group second :one-of (contains? current-pos))))
                     (throw-one-of)
                     (dispose)))
               `(if (contains? ~m ~key)
                  ~(throw-ambiguous)
                  ~(dispose))))

             :else
             (gen-group-branch* (conj prev fst) (subvec group 1))))))))

(defn- gen-group-branch
  ([k target-fn args group]
   (assert (every? args (map :key group))
           (str "Missing entry for keys: " (set/difference (set (map :key group))
                                                           (set (keys args)))))
   (let [meth (let [[fst & nxt] (-> k
                                    name
                                    (str/split #"-"))]
                            (symbol (apply str "." fst (map str/capitalize nxt))))
         m (gensym 'm)
         arg-impl-map (into {}
                            (map (fn [[key {:keys [default] :as arg}]]
                                   {:pre [(keyword? key)]}
                                   [key (let [v `(get ~m ~key ~default)]
                                          (if-let [[_ coerce] (find arg :coerce)]
                                            `(~coerce ~v)
                                            v))]))
                            args)]
     `(fn [~m]
        ~(gen-group-branch* m
                            meth
                            (target-fn m k)
                            arg-impl-map
                            []
                            group)))))

(defn- testfx-target-fn [m k]
  (let [subcontext (keyword (namespace k))
        robot (with-meta (gensym 'robot)
                         {:tag 'org.testfx.api.FxRobot})]
    `(let [~robot (:testfx/robot ~m)
           target# ~(if (not= subcontext :fx-robot)
                      `(from-context ~robot ~subcontext)
                      robot)]
       target#)))

(defn- check-ambiguous-groups [groups]
  (assert (#{1} (count groups))
          "Ambiguous grouping"))

(defn- gen-exec-specs* [target-fn spec-args]
  (into {}
        (map (fn [[k v]]
               (let [{:keys [args arg-groups]} (normalize-exec v)
                     _ (check-ambiguous-groups arg-groups)
                     impl (gen-group-branch k target-fn args (first arg-groups))]
                 [k impl])))
        (partition 2 spec-args)))

(defmacro testfx-specs [& spec-args]
  (gen-exec-specs* testfx-target-fn spec-args))

(defmacro ^:private defn-acoerce [name type coerce pred]
  (let [class-sym (symbol (.getName ^Class (resolve type)))
        tag (str "[L" class-sym ";")
        name (with-meta name {:tag tag})]
    `(let [t# (class (into-array ~class-sym []))]
       (defn ~name [a#]
         (if (instance? t# a#)
           a#
           (into-array ~class-sym (map ~coerce (if (~pred a#)
                                                 #{a#}
                                                 a#))))))))

(def ^KeyCode coerce-key-code (coerce/enum KeyCode))

(defn-acoerce ^:private coerce-mouse-buttons MouseButton coerce-mouse-button keyword?)
(defn-acoerce ^:private coerce-key-codes KeyCode (coerce/enum KeyCode) keyword?)
(defn-acoerce ^:private coerce-nodes Node identity (constantly false))

(def exec-specs
  (testfx-specs
    :base-robot/press-mouse [{:key :button
                              :default :primary
                              :coerce coerce-mouse-button}]
    :base-robot/release-mouse [{:key :button
                                :default :primary
                                :coerce coerce-mouse-button}]
    :base-robot/scroll-mouse [{:key :amount
                               :coerce int}]
    :base-robot/move-mouse [{:key :point
                             :coerce coerce-point2d}]
    :base-robot/capture-region [{:key :region
                                 :coerce coerce/rectangle-2d}]
    :base-robot/retrieve-mouse []

    :base-robot/press-keyboard [{:key :key
                                 :coerce (coerce/enum KeyCode)}]
    :base-robot/release-keyboard [{:key :key
                                   :coerce (coerce/enum KeyCode)}]

    :base-robot/type-keyboard [{:key :scene}
                               {:key :key
                                :coerce (coerce/enum KeyCode)}
                               {:key :character
                                :coerce str}]
    :mouse-robot/press [{:key :buttons
                         :coerce coerce-mouse-buttons
                         :default #{:primary}}]
    :mouse-robot/press-no-wait [{:key :buttons
                                 :coerce coerce-mouse-buttons
                                 :default :primary}]
    :mouse-robot/get-pressed-buttons []
    :mouse-robot/release [{:key :buttons
                           :coerce coerce-mouse-buttons
                           :default :primary}]
    :mouse-robot/release-no-wait [{:key :buttons
                                   :coerce coerce-mouse-buttons
                                   :default :primary}]
    :mouse-robot/move [{:key :location
                        :coerce coerce-point2d}]
    :mouse-robot/move-no-wait [{:key :location
                                :coerce coerce-point2d}]
    :mouse-robot/scroll [{:key :wheel-amount
                          :coerce int}]
    :mouse-robot/scroll-no-wait [{:key :wheel-amount
                                  :coerce int}]

    :keyboard-robot/press [{:key :keys :coerce coerce-key-codes}]
    :keyboard-robot/press-no-wait [{:key :keys :coerce coerce-key-codes}]
    :keyboard-robot/get-pressed-keys []
    :keyboard-robot/release [{:key :keys :coerce coerce-key-codes}]
    :keyboard-robot/release-no-wait [{:key :keys :coerce coerce-key-codes}]

    :drag-robot/drag [{:key :point-query
                       :optional #{0}}
                      {:key :buttons
                       :coerce coerce-mouse-buttons}]
    :drag-robot/drop []
    :drag-robot/drop-to [{:key :point-query}]
    :drag-robot/drop-by [{:key :x :coerce double}
                         {:key :y :coerce double}]
    
    :click-robot/click-on [{:key :point-query
                            :optional #{0}}
                           {:key :motion
                            :coerce coerce-motion
                            :optional #{1}}
                           {:key :buttons
                            :coerce coerce-mouse-buttons}]

    :click-robot/double-click-on [{:key :point-query
                                   :optional #{0}}
                                  {:key :motion
                                   :coerce coerce-motion
                                   :optional #{1}}
                                  {:key :buttons
                                   :coerce coerce-mouse-buttons}]
    
    :window-finder/target-window [{:key :window
                                   :coerce ^javafx.stage.Window identity
                                   :one-of #{0}}
                                  {:key :predicate
                                   :coerce ^java.util.function.Predicate identity
                                   :one-of #{0}}
                                  {:key :window-index
                                   :coerce int
                                   :one-of #{0}}
                                  {:key :stage-title-regex
                                   :coerce str
                                   :one-of #{0}}
                                  {:key :stage-title-pattern
                                   :coerce ^java.util.regex.Pattern identity
                                   :one-of #{0}}
                                  {:key :scene
                                   :coerce ^Scene identity
                                   :one-of #{0}}
                                  {:key :node
                                   :coerce ^Node identity
                                   :one-of #{0}}]

    :window-finder/list-windows []
    :window-finder/list-target-windows []

    :window-finder/window [{:key :predicate
                            :coerce ^java.util.function.Predicate identity
                            :one-of #{0}}
                           {:key :window-index
                            :coerce int
                            :one-of #{0}}
                           {:key :stage-title-regex
                            :coerce str
                            :one-of #{0}}
                           {:key :stage-title-pattern
                            :coerce ^java.util.regex.Pattern identity
                            :one-of #{0}}
                           {:key :scene
                            :coerce ^Scene identity
                            :one-of #{0}}
                           {:key :node
                            :coerce ^Node identity
                            :one-of #{0}}]
    
    :node-finder/lookup [{:key :query
                          :coerce str
                          :one-of #{0}}
                         {:key :matcher
                          :coerce ^org.hamcrest.Matcher identity
                          :one-of #{0}}
                         {:key :predicate
                          :coerce ^java.util.function.Predicate identity
                          :one-of #{0}}]


    :node-finder/from-all []

    :node-finder/from [; Note: overloaded Collection arity is identical, so omitted
                       {:key :parent-nodes
                        :coerce coerce-nodes
                        :one-of #{0}}
                       {:key :node-query
                        :coerce ^org.testfx.service.query.NodeQuery identity
                        :one-of #{0}}]
    :node-finder/root-node [{:key :window
                             :coerce ^javafx.stage.Window identity 
                             :one-of #{0}}
                            {:key :scene
                             :coerce ^Scene identity 
                             :one-of #{0}}
                            {:key :node
                             :coerce ^Node identity 
                             :one-of #{0}}]

    :bounds-locator/bounds-in-scene-for [{:key :node}]
    :bounds-locator/bounds-in-window-for [{:key :bounds-in-scene
                                           :coerce coerce-bounds
                                           :optional #{0}}
                                          {:key :scene}]
    :bounds-locator/bounds-on-screen-for [{:key :node
                                           :coerce ^Node identity
                                           :one-of #{0}}
                                          {:key :scene
                                           :coerce ^Scene identity
                                           :one-of #{0}}
                                          {:key :window
                                           :coerce ^javafx.stage.Window identity
                                           :one-of #{0}}
                                          {:key :bounds-in-scene
                                           :coerce coerce-bounds
                                           :one-of #{0}}
                                          ;TODO test :only-with works
                                          {:key :scene
                                           :only-with #{:bounds-in-scene}}]

    :point-locator/point [{:key :bounds
                           :coerce coerce-bounds
                           :one-of #{0}}
                          {:key :point
                           :coerce coerce-point2d
                           :one-of #{0}}
                          {:key :node
                           :coerce ^Node identity
                           :one-of #{0}}
                          {:key :scene
                           :coerce ^Scene identity
                           :one-of #{0}}
                          {:key :window
                           :coerce ^javafx.stage.Window identity
                           :one-of #{0}}]

    :move-robot/move-to [{:key :point-query}
                         {:key :motion
                          :default :default
                          :coerce coerce-motion}]
    :move-robot/move-by [{:key :x :coerce double}
                         {:key :y :coerce double}
                         {:key :motion
                          :default :default
                          :coerce coerce-motion}]

    :sleep-robot/sleep [{:key :milliseconds
                         :coerce long
                         :one-of #{0}}
                        {:key :duration
                         :coerce long
                         :one-of #{0}}
                        {:key :time-unit
                         :only-with #{:duration}}]


    :scroll-robot/scroll [{:key :amount
                           :coerce int
                           :one-of #{0}}
                          {:key :positive-amount
                           :coerce int
                           :one-of #{0}}
                          ; called `direction` in JavaFX
                          {:key :horizonal-direction
                           :coerce ^javafx.geometry.HorizontalDirection identity
                           :only-with #{:positive-amount}
                           :one-of #{1}}
                          ; called `direction` in JavaFX
                          {:key :vertical-direction
                           :coerce ^javafx.geometry.VerticalDirection identity
                           :only-with #{:positive-amount}
                           :one-of #{1}}]

    :scroll-robot/scroll-up [{:key :positive-amount :coerce int}]
    :scroll-robot/scroll-down [{:key :positive-amount :coerce int}]
    :scroll-robot/scroll-left [{:key :positive-amount :coerce int}]
    :scroll-robot/scroll-right [{:key :positive-amount :coerce int}]

    :type-robot/push [; Note: called `combinations` in TestFX
                      {:key :key-codes
                       :coerce coerce-key-codes
                       :one-of #{0}}
                      ; Note: called `combinations` in TestFX
                      {:key :key-code-combination
                       :coerce ^javafx.scene.input.KeyCodeCombination identity
                       :one-of #{0}}]
    :type-robot/type [{:key :key-codes
                       :coerce coerce-key-codes
                       :one-of #{0}}
                      {:key :key-code
                       :coerce (coerce/enum KeyCode)
                       :one-of #{0}}
                      {:key :times
                       :coerce int
                       :only-with #{:key-code}}]


    :write-robot/write [{:key :character
                         :coerce char
                         :one-of #{0}}
                        {:key :text
                         :coerce str
                         :one-of #{0}}
                        {:key :sleep-millis
                         :coerce int
                         :only-with #{:text}}]

    :capture-support/capture-node [{:key :node}]
    :capture-support/capture-region [{:key :region
                                      :coerce coerce/rectangle-2d}]
    :capture-support/load-image [{:key :path}]
    :capture-support/save-image [{:key :image}
                                 {:key :path}]
    
    ;Note: NYI in TextFX
    :capture-support/annotate-image [{:key :image}
                                     {:key :path}]
    :capture-support/match-images [{:key :image0}
                                   {:key :image1}
                                   {:key :pixel-matcher}]

    :fx-robot/target-window [{:key :window
                              :coerce ^javafx.stage.Window identity
                              :one-of #{0}}
                             {:key :predicate
                              :coerce ^java.util.function.Predicate identity
                              :one-of #{0}}
                             {:key :window-index
                              :coerce int
                              :one-of #{0}}
                             {:key :stage-title-regex
                              :coerce str
                              :one-of #{0}}
                             {:key :stage-title-pattern
                              :coerce ^java.util.regex.Pattern identity
                              :one-of #{0}}
                             {:key :scene
                              :coerce ^Scene identity
                              :one-of #{0}}
                             {:key :node
                              :coerce ^Node identity
                              :one-of #{0}}]
    :fx-robot/list-windows []
    :fx-robot/list-target-windows []

    :fx-robot/window [{:key :predicate
                       :coerce ^java.util.function.Predicate identity
                       :one-of #{0}}
                      {:key :window-index
                       :coerce int
                       :one-of #{0}}
                      {:key :stage-title-regex
                       :coerce str
                       :one-of #{0}}
                      {:key :stage-title-pattern
                       :coerce ^java.util.regex.Pattern identity
                       :one-of #{0}}
                      {:key :scene
                       :coerce ^Scene identity
                       :one-of #{0}}
                      {:key :node
                       :coerce ^Node identity
                       :one-of #{0}}]
    :fx-robot/from-all []
    :fx-robot/from [; Note: overloaded Collection arity is identical, so omitted
                       {:key :parent-nodes
                        :coerce coerce-nodes
                        :one-of #{0}}
                       {:key :node-query
                        :coerce ^org.testfx.service.query.NodeQuery identity
                        :one-of #{0}}]
    :fx-robot/lookup [{:key :query
                       :coerce str
                       :one-of #{0}}
                      {:key :matcher
                       :coerce ^org.hamcrest.Matcher identity
                       :one-of #{0}}
                      {:key :predicate
                       :coerce ^java.util.function.Predicate identity
                       :one-of #{0}}]
    :fx-robot/root-node [{:key :window
                          :coerce ^javafx.stage.Window identity 
                          :one-of #{0}}
                         {:key :scene
                          :coerce ^Scene identity 
                          :one-of #{0}}
                         {:key :node
                          :coerce ^Node identity 
                          :one-of #{0}}]
    :fx-robot/bounds [{:key :point
                       :coerce coerce-point2d
                       :one-of #{0}}
                      {:key :bounds
                       :coerce coerce-bounds
                       :one-of #{0}}
                      {:key :node
                       :coerce ^Node identity
                       :one-of #{0}}
                      {:key :scene
                       :coerce ^Scene identity
                       :one-of #{0}}
                      {:key :window
                       :coerce ^javafx.stage.Window identity
                       :one-of #{0}}
                      {:key :min-x
                       :coerce double
                       :one-of #{0}}
                      {:key :min-y
                       :coerce double
                       :only-with #{:min-x}}
                      {:key :width
                       :coerce double
                       :only-with #{:min-y}}
                      {:key :height
                       :coerce double
                       :only-with #{:width}}]
    :fx-robot/target-pos [{:key :point-position
                           :coerce ^javafx.geometry.Pos identity}]
    :fx-robot/point [{:key :point
                      :coerce coerce-point2d
                      :one-of #{0}}
                     {:key :bounds
                      :coerce coerce-bounds
                      :one-of #{0}}
                     {:key :node
                      :coerce ^Node identity
                      :one-of #{0}}
                     {:key :scene
                      :coerce ^Scene identity
                      :one-of #{0}}
                     {:key :window
                      :coerce ^javafx.stage.Window identity
                      :one-of #{0}}
                     {:key :query
                      :coerce str
                      :one-of #{0}}
                     {:key :matcher
                      :coerce ^org.hamcrest.Matcher identity
                      :one-of #{0}}
                     {:key :predicate
                      :coerce ^java.util.function.Predicate identity
                      :one-of #{0}}
                     {:key :x
                      :coerce double
                      :one-of #{0}}
                     {:key :y
                      :coerce double
                      :only-with #{:x}}]
    :fx-robot/offset [{:key :point
                       :coerce coerce-point2d
                       :one-of #{0}}
                      {:key :bounds
                       :coerce coerce-bounds
                       :one-of #{0}}
                      {:key :scene
                       :coerce ^Scene identity
                       :one-of #{0}}
                      {:key :window
                       :coerce ^javafx.stage.Window identity
                       :one-of #{0}}
                      {:key :query
                       :coerce str
                       :one-of #{0}}
                      {:key :matcher
                       :coerce ^org.hamcrest.Matcher identity
                       :one-of #{0}}
                      {:key :predicate
                       :coerce ^java.util.function.Predicate identity
                       :one-of #{0}}
                      {:key :node
                       :coerce ^Node identity
                       :one-of #{0}}
                      {:key :offset-reference-pos
                       :coerce ^javafx.geometry.Pos identity
                       :only-with #{:node}}
                      {:key :offset-x
                       :coerce double}
                      {:key :offset-y
                       :coerce double}]
    :fx-robot/capture [{:key :screen-region
                        :coerce coerce/rectangle-2d
                        :one-of #{0}}
                       {:key :bounds
                        :coerce coerce-bounds
                        :one-of #{0}}
                       {:key :node
                        :coerce ^Node identity
                        :one-of #{0}}
                       {:key :image
                        :coerce ^javafx.scene.image.Image identity
                        :one-of #{0}}
                       {:key :path
                        :coerce ^java.nio.file.Path identity
                        :one-of #{0}}
                       {:key :url
                        :coerce ^java.net.URL identity
                        :one-of #{0}}]
    :fx-robot/interact [{:key :runnable
                         :coerce ^Runnable identity
                         :one-of #{0}}
                        {:key :callable
                         :coerce ^Callable identity
                         :one-of #{0}}]
    :fx-robot/interact-no-wait [{:key :runnable
                                 :coerce ^Runnable identity
                                 :one-of #{0}}
                                {:key :callable
                                 :coerce ^Callable identity
                                 :one-of #{0}}]
    :fx-robot/interrupt [{:key :attempts-count
                          :coerce int
                          :optional #{0}}]
    :fx-robot/push [; Note: called `combinations` in TestFX
                      {:key :key-codes
                       :coerce coerce-key-codes
                       :one-of #{0}}
                      ; Note: called `combinations` in TestFX
                      {:key :key-code-combination
                       :coerce ^javafx.scene.input.KeyCodeCombination identity
                       :one-of #{0}}]
    :fx-robot/type [{:key :key-codes
                     :coerce coerce-key-codes
                     :one-of #{0}}
                    {:key :key-code
                     :coerce coerce-key-code
                     :one-of #{0}}
                    {:key :times
                     :coerce int
                     :only-with #{:key-code}}]
    :fx-robot/erase-text [{:key :amount
                           :coerce int}]

    ;deprecated
    ;:fx-robot/close-current-window

    :fx-robot/write [{:key :character
                      :coerce char
                      :one-of #{0}}
                     {:key :text
                      :coerce str
                      :one-of #{0}}
                     {:key :sleep-millis
                      :coerce int
                      :only-with #{:text}}]
    :fx-robot/sleep [{:key :milliseconds
                      :coerce long
                      :one-of #{0}}
                     {:key :duration
                      :coerce long
                      :one-of #{0}}
                     {:key :time-unit
                      :coerce ^java.util.concurrent.TimeUnit identity
                      :only-with #{:duration}}]
    ;Note: Omitted direction 1-arities, simulated by :amount default
    :fx-robot/scroll [{:key :amount
                       :coerce int
                       :default 1}
                      ; called `direction` in JavaFX
                      {:key :horizonal-direction
                       :coerce ^javafx.geometry.HorizontalDirection identity
                       :optional #{1}}
                      ; called `direction` in JavaFX
                      {:key :vertical-direction
                       :coerce ^javafx.geometry.VerticalDirection identity
                       :optional #{1}}]
    :fx-robot/press [{:key :keys
                      :coerce coerce-key-codes
                      :one-of #{0}}
                     {:key :buttons
                      :coerce coerce-mouse-buttons
                      :one-of #{0}}]
    :fx-robot/release [{:key :keys
                        :coerce coerce-key-codes
                        :one-of #{0}}
                       {:key :buttons
                        :coerce coerce-mouse-buttons
                        :one-of #{0}}]
    :fx-robot/click-on {:args {; :buttons is referenced twice
                               :buttons {:coerce coerce-mouse-buttons}
                               :point-query {:coerce ^org.testfx.service.query.PointQuery identity}
                               :motion {:coerce coerce-motion}
                               :point {:point coerce-point2d}
                               :bounds {:coerce coerce-bounds}
                               :node {:coerce ^Node identity}
                               :scene {:coerce ^Scene identity}
                               :window {:coerce ^javafx.stage.Window identity}
                               :query {:coerce str}
                               :matcher {:coerce ^org.hamcrest.Matcher identity}
                               :predicate {:coerce ^java.util.function.Predicate identity}
                               :x {:coerce double}
                               :y {:coerce double}}
                        :arg-groups #{[{:key :buttons
                                        :one-of #{0}}
                                       {:key :point-query
                                        :one-of #{0}}
                                       {:key :point
                                        :one-of #{0}}
                                       {:key :bounds
                                        :one-of #{0}}
                                       {:key :node
                                        :one-of #{0}}
                                       {:key :scene
                                        :one-of #{0}}
                                       {:key :window
                                        :one-of #{0}}
                                       {:key :query
                                        :one-of #{0}}
                                       {:key :matcher
                                        :one-of #{0}}
                                       {:key :predicate
                                        :one-of #{0}}
                                       {:key :x
                                        :one-of #{0}}
                                       {:key :y
                                        :only-with #{:x}}
                                       {:key :motion
                                        ; allow 1-arg :buttons
                                        :only-with [#{:point-query}
                                                    #{:x :y}
                                                    #{:point}
                                                    #{:bounds}
                                                    #{:node}
                                                    #{:scene}
                                                    #{:window}
                                                    #{:query}
                                                    #{:matcher}
                                                    #{:predicate}]}
                                       {:key :buttons
                                        :only-with #{:motion}}]}}
    :fx-robot/right-click-on [{:key :point-query
                               :coerce ^org.testfx.service.query.PointQuery identity
                               :optional #{0}}
                              {:key :point
                               :point coerce-point2d
                               :optional #{0}}
                              {:key :bounds
                               :coerce coerce-bounds
                               :optional #{0}}
                              {:key :node
                               :coerce ^Node identity
                               :optional #{0}}
                              {:key :scene
                               :coerce ^Scene identity
                               :optional #{0}}
                              {:key :window
                               :coerce ^javafx.stage.Window identity
                               :optional #{0}}
                              {:key :query
                               :coerce str
                               :optional #{0}}
                              {:key :matcher
                               :coerce ^org.hamcrest.Matcher identity
                               :optional #{0}}
                              {:key :predicate
                               :coerce ^java.util.function.Predicate identity
                               :optional #{0}}
                              {:key :x
                               :coerce double
                               :optional #{0}}
                              {:key :y
                               :coerce double
                               :only-with #{:x}}
                              {:key :motion
                               :coerce coerce-motion
                               ; allow zero-arg case, but otherwise required
                               :only-with [#{:point-query}
                                           #{:x :y}
                                           #{:point}
                                           #{:bounds}
                                           #{:node}
                                           #{:scene}
                                           #{:window}
                                           #{:query}
                                           #{:matcher}
                                           #{:predicate}]}]
    :fx-robot/double-click-on [{:key :point-query
                                :coerce ^org.testfx.service.query.PointQuery identity
                                :one-of #{0}}
                               {:key :point
                                :point coerce-point2d
                                :one-of #{0}}
                               {:key :bounds
                                :coerce coerce-bounds
                                :one-of #{0}}
                               {:key :node
                                :coerce ^Node identity
                                :one-of #{0}}
                               {:key :scene
                                :coerce ^Scene identity
                                :one-of #{0}}
                               {:key :window
                                :coerce ^javafx.stage.Window identity
                                :one-of #{0}}
                               {:key :query
                                :coerce str
                                :one-of #{0}}
                               {:key :matcher
                                :coerce ^org.hamcrest.Matcher identity
                                :one-of #{0}}
                               {:key :predicate
                                :coerce ^java.util.function.Predicate identity
                                :one-of #{0}}
                               {:key :x
                                :coerce double
                                :one-of #{0}}
                               {:key :y
                                :coerce double
                                :only-with #{:x}}
                               {:key :motion
                                :coerce coerce-motion}
                               {:key :buttons
                                :coerce coerce-mouse-buttons}]
    :fx-robot/drag [{:key :point-query
                     :coerce ^org.testfx.service.query.PointQuery identity
                     :optional #{0}}
                    {:key :point
                     :point coerce-point2d
                     :optional #{0}}
                    {:key :bounds
                     :coerce coerce-bounds
                     :optional #{0}}
                    {:key :node
                     :coerce ^Node identity
                     :optional #{0}}
                    {:key :scene
                     :coerce ^Scene identity
                     :optional #{0}}
                    {:key :window
                     :coerce ^javafx.stage.Window identity
                     :optional #{0}}
                    {:key :query
                     :coerce str
                     :optional #{0}}
                    {:key :matcher
                     :coerce ^org.hamcrest.Matcher identity
                     :optional #{0}}
                    {:key :predicate
                     :coerce ^java.util.function.Predicate identity
                     :optional #{0}}
                    {:key :x
                     :coerce double
                     :optional #{0}}
                    {:key :y
                     :coerce double
                     :only-with #{:x}}
                    ; has 1-arity version with just buttons
                    {:key :buttons
                     :coerce coerce-mouse-buttons}]
    :fx-robot/drop []
    :fx-robot/drop-to [{:key :point-query
                        :coerce ^org.testfx.service.query.PointQuery identity
                        :one-of #{0}}
                       {:key :point
                        :coerce coerce-point2d
                        :one-of #{0}}
                       {:key :bounds
                        :coerce coerce-bounds
                        :one-of #{0}}
                       {:key :node
                        :coerce ^Node identity
                        :one-of #{0}}
                       {:key :scene
                        :coerce ^Scene identity
                        :one-of #{0}}
                       {:key :window
                        :coerce ^javafx.stage.Window identity
                        :one-of #{0}}
                       {:key :query
                        :coerce str
                        :one-of #{0}}
                       {:key :matcher
                        :coerce ^org.hamcrest.Matcher identity
                        :one-of #{0}}
                       {:key :predicate
                        :coerce ^java.util.function.Predicate identity
                        :one-of #{0}}
                       {:key :x
                        :coerce double
                        :one-of #{0}}
                       {:key :y
                        :coerce double
                        :only-with #{:x}}]

    :fx-robot/drop-by [{:key :x :coerce double}
                       {:key :y :coerce double}]

    :fx-robot/move-to [{:key :point-query
                        :coerce ^org.testfx.service.query.PointQuery identity
                        :one-of #{0}}
                       {:key :point
                        :coerce coerce-point2d
                        :one-of #{0}}
                       {:key :bounds
                        :coerce coerce-bounds
                        :one-of #{0}}
                       {:key :node
                        :coerce ^Node identity
                        :one-of #{0}}
                       {:key :scene
                        :coerce ^Scene identity
                        :one-of #{0}}
                       {:key :window
                        :coerce ^javafx.stage.Window identity
                        :one-of #{0}}
                       {:key :query
                        :coerce str
                        :one-of #{0}}
                       {:key :matcher
                        :coerce ^org.hamcrest.Matcher identity
                        :one-of #{0}}
                       {:key :predicate
                        :coerce ^java.util.function.Predicate identity
                        :one-of #{0}}
                       {:key :x
                        :coerce double
                        :one-of #{0}}
                       {:key :y
                        :coerce double
                        :only-with #{:x}}
                       {:key :offset-reference-pos
                        :only-with #{:node}}
                       {:key :offset
                        :only-with #{:node}}
                       {:key :motion
                        :default :default
                        :coerce coerce-motion}]
))

(defn exec [robot spec]
  (((:testfx/op spec) exec-specs)
   (assoc spec :testfx/robot robot)))

(def ^:private keyword->class-sym
  '{:sphere javafx.scene.shape.Sphere,
    :indexed-cell javafx.scene.control.IndexedCell,
    :svg-path javafx.scene.shape.SVGPath,
    :path javafx.scene.shape.Path,
    :html-editor javafx.scene.web.HTMLEditor,
    :text-area javafx.scene.control.TextArea,
    :pie-chart javafx.scene.chart.PieChart,
    :parallel-camera javafx.scene.ParallelCamera,
    :tile-pane javafx.scene.layout.TilePane,
    :table-view javafx.scene.control.TableView,
    :bloom javafx.scene.effect.Bloom,
    :gaussian-blur javafx.scene.effect.GaussianBlur,
    :point-light javafx.scene.PointLight,
    :progress-bar javafx.scene.control.ProgressBar,
    :combo-box javafx.scene.control.ComboBox,
    :pane javafx.scene.layout.Pane,
    :popup javafx.stage.Popup,
    :menu-item javafx.scene.control.MenuItem,
    :anchor-pane javafx.scene.layout.AnchorPane,
    :custom-menu-item javafx.scene.control.CustomMenuItem,
    :group javafx.scene.Group,
    :media-view javafx.scene.media.MediaView,
    :stage javafx.stage.Stage,
    :arc javafx.scene.shape.Arc,
    :light-spot javafx.scene.effect.Light$Spot,
    :affine javafx.scene.transform.Affine,
    :shear javafx.scene.transform.Shear,
    :radio-menu-item javafx.scene.control.RadioMenuItem,
    :close-path javafx.scene.shape.ClosePath,
    :list-spinner-value-factory javafx.scene.control.SpinnerValueFactory$ListSpinnerValueFactory,
    :number-axis javafx.scene.chart.NumberAxis,
    :scale javafx.scene.transform.Scale,
    :button javafx.scene.control.Button,
    :lighting javafx.scene.effect.Lighting,
    :tool-bar javafx.scene.control.ToolBar,
    :bar-chart javafx.scene.chart.BarChart,
    :xy-chart-series javafx.scene.chart.XYChart$Series,
    :radio-button javafx.scene.control.RadioButton,
    :perspective-camera javafx.scene.PerspectiveCamera,
    :move-to javafx.scene.shape.MoveTo,
    :text-input-dialog javafx.scene.control.TextInputDialog,
    :split-menu-button javafx.scene.control.SplitMenuButton,
    :rotate-transition javafx.animation.RotateTransition,
    :pause-transition javafx.animation.PauseTransition,
    :grid-pane javafx.scene.layout.GridPane,
    :combo-box-list-cell javafx.scene.control.cell.ComboBoxListCell,
    :sub-scene javafx.scene.SubScene,
    :scale-transition javafx.animation.ScaleTransition,
    :text-flow javafx.scene.text.TextFlow,
    :box javafx.scene.shape.Box,
    :cubic-curve javafx.scene.shape.CubicCurve,
    :quad-curve-to javafx.scene.shape.QuadCurveTo,
    :circle javafx.scene.shape.Circle,
    :motion-blur javafx.scene.effect.MotionBlur,
    :line-to javafx.scene.shape.LineTo,
    :h-line-to javafx.scene.shape.HLineTo,
    :popup-control javafx.scene.control.PopupControl,
    :drop-shadow javafx.scene.effect.DropShadow,
    :flow-pane javafx.scene.layout.FlowPane,
    :stacked-bar-chart javafx.scene.chart.StackedBarChart,
    :alert javafx.scene.control.Alert,
    :mesh-view javafx.scene.shape.MeshView,
    :color-adjust javafx.scene.effect.ColorAdjust,
    :progress-indicator javafx.scene.control.ProgressIndicator,
    :text-field javafx.scene.control.TextField,
    :tree-view javafx.scene.control.TreeView,
    :menu-bar javafx.scene.control.MenuBar,
    :fill-transition javafx.animation.FillTransition,
    :color-picker javafx.scene.control.ColorPicker,
    :image-input javafx.scene.effect.ImageInput,
    :category-axis javafx.scene.chart.CategoryAxis,
    :stack-pane javafx.scene.layout.StackPane,
    :xy-chart-data javafx.scene.chart.XYChart$Data,
    :tree-item javafx.scene.control.TreeItem,
    :accordion javafx.scene.control.Accordion,
    :spinner javafx.scene.control.Spinner,
    :v-box javafx.scene.layout.VBox,
    :double-spinner-value-factory javafx.scene.control.SpinnerValueFactory$DoubleSpinnerValueFactory,
    :canvas javafx.scene.canvas.Canvas,
    :password-field javafx.scene.control.PasswordField,
    :v-line-to javafx.scene.shape.VLineTo,
    :region javafx.scene.layout.Region,
    :displacement-map javafx.scene.effect.DisplacementMap,
    :cubic-curve-to javafx.scene.shape.CubicCurveTo,
    :scroll-pane javafx.scene.control.ScrollPane,
    :triangle-mesh javafx.scene.shape.TriangleMesh,
    :split-pane javafx.scene.control.SplitPane,
    :toggle-button javafx.scene.control.ToggleButton,
    :date-picker javafx.scene.control.DatePicker,
    :table-column javafx.scene.control.TableColumn,
    :web-view javafx.scene.web.WebView,
    :line javafx.scene.shape.Line,
    :list-view javafx.scene.control.ListView,
    :choice-box javafx.scene.control.ChoiceBox,
    :bubble-chart javafx.scene.chart.BubbleChart,
    :stacked-area-chart javafx.scene.chart.StackedAreaChart,
    :pagination javafx.scene.control.Pagination,
    :media-player javafx.scene.media.MediaPlayer,
    :quad-curve javafx.scene.shape.QuadCurve,
    :ambient-light javafx.scene.AmbientLight,
    :check-box javafx.scene.control.CheckBox,
    :label javafx.scene.control.Label,
    :image-view javafx.scene.image.ImageView,
    :color-input javafx.scene.effect.ColorInput,
    :cylinder javafx.scene.shape.Cylinder,
    :border-pane javafx.scene.layout.BorderPane,
    :row-constraints javafx.scene.layout.RowConstraints,
    :context-menu javafx.scene.control.ContextMenu,
    :integer-spinner-value-factory javafx.scene.control.SpinnerValueFactory$IntegerSpinnerValueFactory,
    :translate-transition javafx.animation.TranslateTransition,
    :ellipse javafx.scene.shape.Ellipse,
    :pie-chart-data javafx.scene.chart.PieChart$Data,
    :inner-shadow javafx.scene.effect.InnerShadow,
    :light-distant javafx.scene.effect.Light$Distant,
    :stroke-transition javafx.animation.StrokeTransition,
    :translate javafx.scene.transform.Translate,
    :dialog javafx.scene.control.Dialog,
    :rotate javafx.scene.transform.Rotate,
    :sequential-transition javafx.animation.SequentialTransition,
    :hyperlink javafx.scene.control.Hyperlink,
    :tab-pane javafx.scene.control.TabPane,
    :column-constraints javafx.scene.layout.ColumnConstraints,
    :button-bar javafx.scene.control.ButtonBar,
    :blend javafx.scene.effect.Blend,
    :path-transition javafx.animation.PathTransition,
    :box-blur javafx.scene.effect.BoxBlur,
    :fade-transition javafx.animation.FadeTransition,
    :menu-button javafx.scene.control.MenuButton,
    :area-chart javafx.scene.chart.AreaChart,
    :choice-dialog javafx.scene.control.ChoiceDialog,
    :h-box javafx.scene.layout.HBox,
    :tree-table-column javafx.scene.control.TreeTableColumn,
    :shadow javafx.scene.effect.Shadow,
    :tab javafx.scene.control.Tab,
    :separator javafx.scene.control.Separator,
    :check-menu-item javafx.scene.control.CheckMenuItem,
    :dialog-pane javafx.scene.control.DialogPane,
    :perspective-transform javafx.scene.effect.PerspectiveTransform,
    :arc-to javafx.scene.shape.ArcTo,
    :cell javafx.scene.control.Cell,
    :polyline javafx.scene.shape.Polyline,
    :phong-material javafx.scene.paint.PhongMaterial,
    :media javafx.scene.media.Media,
    :glow javafx.scene.effect.Glow,
    :parallel-transition javafx.animation.ParallelTransition,
    :toggle-group javafx.scene.control.ToggleGroup,
    :scatter-chart javafx.scene.chart.ScatterChart,
    :reflection javafx.scene.effect.Reflection,
    :menu javafx.scene.control.Menu,
    :tooltip javafx.scene.control.Tooltip,
    :rectangle javafx.scene.shape.Rectangle,
    :tree-table-view javafx.scene.control.TreeTableView,
    :list-cell javafx.scene.control.ListCell,
    :line-chart javafx.scene.chart.LineChart,
    :text-formatter javafx.scene.control.TextFormatter,
    :slider javafx.scene.control.Slider,
    :polygon javafx.scene.shape.Polygon,
    :sepia-tone javafx.scene.effect.SepiaTone,
    :scene javafx.scene.Scene,
    :titled-pane javafx.scene.control.TitledPane,
    :text javafx.scene.text.Text,
    :scroll-bar javafx.scene.control.ScrollBar,
    :light-point javafx.scene.effect.Light$Point})

(assert (= (set (keys keyword->class-sym))
           (set (keys cljfx.fx/keyword->lifecycle-delay))))

#_
(clojure.pprint/pprint (zipmap (keys cljfx.fx/keyword->lifecycle-delay)
             (repeat 'javafx)))

#_(getter :button :text)


(defmacro getter [cls meth & args]
  (let [instance-sym (with-meta (gensym "instance") {:tag (if (keyword? cls)
                                                            (keyword->class-sym cls)
                                                            cls)})
        getter-expr (if (keyword? meth)
                      (symbol (apply str ".get" (map str/capitalize (-> meth
                                                                        name
                                                                        (str/split #"-")))))
                      meth)]
    `(fn [~instance-sym]
       (~getter-expr ~instance-sym ~@args))))

;(getter :button .getText)
;(getter :button :text)
;(getter :button :tex)
;(getter :button .asdf)

;{:testfx/assert :button
; :button {:textfx/query :button
;          :lookup ".button"}
; :checks [:is-cancel-button
;          :is-not-cancel-button
;          :is-default-button
;          :is-not-default-button
;          ]
; }

(comment
(def fx-packages
  '[javafx.animation
    javafx.application
    javafx.beans
    javafx.beans.binding
    javafx.beans.property
    javafx.beans.property.adapter
    javafx.beans.value
    javafx.collections
    javafx.concurrent
    javafx.embed.swing
    javafx.embed.swt
    javafx.event
    javafx.fxml
    javafx.geometry
    javafx.scene
    javafx.scene.canvas
    javafx.scene.chart
    javafx.scene.control
    javafx.scene.control.cell
    javafx.scene.effect
    javafx.scene.image
    javafx.scene.input
    javafx.scene.layout
    javafx.scene.media
    javafx.scene.paint
    javafx.scene.shape
    javafx.scene.text
    javafx.scene.transform
    javafx.scene.web
    javafx.stage
    javafx.util
    javafx.util.converter
    netscape.javascript])

(defn find-fx-classes []
  (into {}
        (map (fn [kw]
               (let [simple-str (case kw
                                  :svg-path "SVGPath"
                                  :html-editor "HTMLEditor"
                                  :light-spot "Light$Spot"
                                  :list-spinner-value-factory "SpinnerValueFactory$ListSpinnerValueFactory"
                                  :xy-chart-series "XYChart$Series"
                                  :xy-chart-data "XYChart$Data"
                                  :double-spinner-value-factory "SpinnerValueFactory$DoubleSpinnerValueFactory"
                                  :integer-spinner-value-factory "SpinnerValueFactory$IntegerSpinnerValueFactory"
                                  :pie-chart-data "PieChart$Data"
                                  :light-distant "Light$Distant"
                                  :light-point "Light$Point"
                                  (apply str (map str/capitalize (-> kw
                                                                     name
                                                                     (str/split #"-")))))
                     full-sym (some (fn [pkg]
                                      (let [cls-str (str pkg "." simple-str)]
                                        (when (try (Class/forName cls-str)
                                                   (catch Exception _))
                                          (symbol cls-str))))
                                    fx-packages)]
                 (assert full-sym kw)
                 [kw full-sym]
                 )))
        (keys cljfx.fx/keyword->lifecycle-delay)
       ))

(require 'clojure.pprint)

(clojure.pprint/pprint (find-fx-classes))
)
