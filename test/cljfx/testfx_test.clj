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

(defn general-spec-target-fn [m k]
  (let [o (with-meta (gensym 'o)
                     {:tag (-> k namespace symbol)})]
    `(let [~o (:target ~m)]
       ~o)))

(defmacro tst-general-specs [& spec-args]
  (@#'cljfx.testfx/gen-exec-specs*
    general-spec-target-fn
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
    (try
      (tst {:str "a"
            :ch \a})
      (is false)
      (catch clojure.lang.ExceptionInfo e
        (is (= {:keys #{:str :ch}} (select-keys (ex-data e) [:keys])))))
    (try
      (tst {:str "a"
            :ch \a})
      (is false)
      (catch clojure.lang.ExceptionInfo e
        (is (= "Disallowed key combination" (ex-message e)))
        (is (= {:keys #{:str :ch}} (select-keys (ex-data e) [:keys])))))
    ))
