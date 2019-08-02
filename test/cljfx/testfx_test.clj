(ns cljfx.testfx-test
  (:require [clojure.test :refer :all]
            [cljfx.api :as fx]
            [cljfx.testfx :as testfx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.pprint :as pprint])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [org.testfx.robot Motion BaseRobot]
           [javafx.geometry Point2D Bounds]
           [javafx.scene Node Scene]
           [javafx.scene.input MouseButton KeyCode]))

(set! *warn-on-reflection* true)

(defn general-spec-meth-fn [k]
  (let [[fst & nxt] (-> k
                        name
                        (str/split #"-"))]
    (symbol (apply str "." fst (map str/capitalize nxt)))))

(defn general-spec-target-fn [m k]
  (let [o (with-meta (gensym 'o)
                     {:tag (-> k namespace symbol)})]
    `(let [~o (:target ~m)]
       ~o)))

(defmacro tst-general-specs [& spec-args]
  (@#'cljfx.testfx/gen-exec-specs*
    general-spec-target-fn
    general-spec-meth-fn
    spec-args))

(def general-specs
  (tst-general-specs
    :java.lang.String/index-of {:args {:ch {:coerce int}
                                       :str {:coerce str}
                                       :from-index {:coerce int}}
                                :arg-groups #{[{:key :ch
                                                :one-of #{0}}
                                               {:key :str
                                                :one-of #{0}}
                                               {:key :from-index
                                                :optional #{1}}]}}
    ))


(defn exec-general-spec [o spec]
  {:pre [(not (contains? spec :target))]}
  (((:general/op spec) general-specs)
   (assoc spec :target o)))

(defmacro throws-ex-info [out ex-info-fn]
  `(try
     ~out
     (is false (str "Should throw: " '~out))
     (catch clojure.lang.ExceptionInfo e#
       (~ex-info-fn e#))
     (catch Throwable e#
       (is false (str "Wrong exception: " (class e#) '~out)))))

(deftest general-spec-test
  (let [tst #(exec-general-spec "asdf"
                                (merge {:general/op :java.lang.String/index-of}
                                       %))]
    (are [x y] (= x y)
         (tst {:ch \a}) 0
         (tst {:ch \a
               :from-index 0}) 0
         (tst {:ch \a
               :from-index 1}) -1
         (tst {:ch \s}) 1
         (tst {:ch \d}) 2
         (tst {:ch \f}) 3
         (tst {:ch \b}) -1
         (tst {:str "a"}) 0
         (tst {:str "as"}) 0
         (tst {:str "asd"}) 0
         (tst {:str "asdf"}) 0
         (tst {:str "asdfb"}) -1
         (tst {:str 'asdf}) 0
         (tst {:str 'asdfb}) -1
         (tst {:str 'sdf
               :from-index 1}) 1
         (tst {:str 'df
               :from-index 2}) 2
         (tst {:str 'df
               :from-index 1}) 2
         (tst {:str 'df
               :from-index 3}) -1
         )
    (throws-ex-info
      (tst {:str "a"
            :ch \a})
      (fn [e]
        (is (= {:keys #{:str :ch}} (select-keys (ex-data e) [:keys])))))
    (throws-ex-info
      (tst {:str "a"
            :ch \a})
      (fn [e]
        (is (= "Disallowed key combination" (ex-message e)))
        (is (= {:keys #{:str :ch}} (select-keys (ex-data e) [:keys])))))
    ))

(defn spec-test-fn [& args]
  (vec args))

(defn clj-spec-meth-fn [k]
  `spec-test-fn)

(defn clj-spec-target-fn [m k]
  `(:target ~m))

(defmacro tst-clj-specs [& spec-args]
  (@#'cljfx.testfx/gen-exec-specs*
    clj-spec-target-fn
    clj-spec-meth-fn
    spec-args))

(def clj-specs
  (tst-clj-specs
    :one-of+optional {:args {:ch {:coerce int}
                             :str {:coerce str}
                             :from-index {:coerce int}}
                      :arg-groups #{[{:key :ch
                                      :one-of #{0}}
                                     {:key :str
                                      :one-of #{0}}
                                     {:key :from-index
                                      :optional #{1}}]}}
    :one-of+only-with {:args {:ch {:coerce int}
                              :str {:coerce str}
                              :from-index {:coerce int}}
                       :arg-groups #{[{:key :ch
                                       :one-of #{0}}
                                      {:key :str
                                       :one-of #{0}}
                                      {:key :from-index
                                       :only-with #{:str}}]}}
    :one-of+optional-only-with {:args {:ch {:coerce int}
                              :str {:coerce str}
                              :from-index {:coerce int}}
                       :arg-groups #{[{:key :ch
                                       :one-of #{0}}
                                      {:key :str
                                       :one-of #{0}}
                                      {:key :from-index
                                       :optional #{1}
                                       :only-with #{:str}}]}}
    :optional+default {:args {:ch {:coerce int}
                              :str {:coerce str}
                              :from-index {:coerce int
                                           :default 1}}
                       :arg-groups #{[{:key :ch
                                       :one-of #{0}}
                                      {:key :str
                                       :one-of #{0}}
                                      {:key :from-index
                                       :optional #{1}
                                       :only-with #{:str}}]}}
    :mandatory-default {:args {:ch {:coerce int}
                               :str {:coerce str}
                               :from-index {:coerce inc
                                            :default 1}}
                        :arg-groups #{[{:key :ch
                                        :one-of #{0}}
                                       {:key :str
                                        :one-of #{0}}
                                       {:key :from-index
                                        :only-with #{:str}}]}}
    ))

(defn exec-clj-spec [o spec]
  {:pre [(not (contains? spec :target))]}
  (((:op spec) clj-specs)
   (assoc spec :target o)))

(deftest clj-spec-test
  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+optional
                         :ch \a})
         ["my-object" 97]))
  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+optional
                         :str "a"})
         ["my-object" "a"]))
  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+optional
                         :str "a"
                         :from-index 1})
         ["my-object" "a" 1]))
  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+optional
                         :ch 0
                         :from-index 1})
         ["my-object" 0 1]))
  (doseq [m [{} {:from-index 1}]]
    (throws-ex-info (exec-clj-spec "my-object"
                                   (merge {:op :one-of+optional} m))
                    (fn [e]
                      (is (= "Must provide one of keys" (ex-message e)))
                      (is (= {:pos 0
                              :keys #{:str}}
                             (ex-data e))))))

  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+only-with
                         :str "a"
                         :from-index 1
                         })
         ["my-object" "a" 1]))
  (throws-ex-info (exec-clj-spec "my-object"
                                 {:op :one-of+only-with
                                  :ch \a
                                  :from-index 1
                                  })
                  (fn [e]
                    (is (= "Key provided without dependency" (ex-message e)))
                    (is (= {:key :from-index
                            :missing-dependency #{:str}
                            :others #{:ch}}
                           (ex-data e)))))
  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+optional-only-with
                         :str "a"
                         })
         ["my-object" "a"]))
  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+optional-only-with
                         :ch 1
                         })
         ["my-object" 1]))
  (is (= (exec-clj-spec "my-object"
                        {:op :one-of+optional-only-with
                         :str "a"
                         :from-index 1
                         })
         ["my-object" "a" 1]))
  (throws-ex-info (exec-clj-spec "my-object"
                                 {:op :one-of+optional-only-with
                                  :ch 1
                                  :from-index 1
                                  })
                  (fn [e]
                    (is (= "Key provided without dependency" (ex-message e)))
                    (is (= {:key :from-index
                            :missing-dependency #{:str}
                            :others #{:ch}}
                           (ex-data e)))))
  (is (= (exec-clj-spec "my-object"
                        {:op :optional+default
                         :str "a"
                         :from-index 1
                         })
         ["my-object" "a" 1]))
  ;the weird case, should :from-index be automatically filled in?
  ; it's :optional, but it has a :default.
  (is (= (exec-clj-spec "my-object"
                        {:op :optional+default
                         :str "a"})
         ["my-object" "a"]))
  (throws-ex-info (exec-clj-spec "my-object"
                                 {:op :optional+default
                                  :ch "a"
                                  :from-index 1
                                  })
                  (fn [e]
                    (is (= "Key provided without dependency" (ex-message e)))
                    (is (= {:key :from-index
                            :missing-dependency #{:str}
                            :others #{:ch}}
                           (ex-data e)))))
  (is (= (exec-clj-spec "my-object"
                        {:op :mandatory-default
                         :ch 1})
         ["my-object" 1]))
  (is (= (exec-clj-spec "my-object"
                        {:op :mandatory-default
                         :str "a"
                         :from-index -1
                         })
         ["my-object" "a" 0]))
  (is (= (exec-clj-spec "my-object"
                        {:op :mandatory-default
                         :str "a"})
         ["my-object" "a" 2]))
  (throws-ex-info (exec-clj-spec "my-object"
                                 {:op :mandatory-default
                                  :ch "a"
                                  :from-index 1
                                  })
                  (fn [e]
                    (is (= "Key provided without dependency" (ex-message e)))
                    (is (= {:key :from-index
                            :missing-dependency #{:str}
                            :others #{:ch}}
                           (ex-data e)))))
  )
