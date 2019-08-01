(ns cljfx.testfx-test
  (:require [clojure.test :refer :all]
            [cljfx.api :as fx]
            [cljfx.testfx :as testfx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce]
            [clojure.string :as str]
            [clojure.test :as test])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [org.testfx.robot Motion BaseRobot]
           [javafx.geometry Point2D Bounds]
           [javafx.scene Node Scene]
           [javafx.scene.input MouseButton KeyCode]))

(defmacro tst-general-specs [& spec-args]
  (@#'cljfx.testfx/gen-exec-specs*
    (fn [m k]
      (let [o (with-meta (gensym 'o)
                         {:tag (-> k namespace symbol)})]
        `(let [~o (:target ~m)]
           ~o)))
    spec-args))

(def general-specs
  (tst-general-specs
    :java.lang.String/index-of [{:key :ch
                                 :coerce int}]))

(defn exec-general-spec [o spec]
  {:pre [(not (contains? spec :target))]}
  (((:general/op spec) general-specs)
   (assoc spec :target o)))

(deftest general-spec-test
  (let [tst #(exec-general-spec "asdf"
                                {:general/op :java.lang.String/index-of
                                 :ch %})]
    (is (= (tst \a) 0))
    (is (= (tst \s) 1))
    (is (= (tst \d) 2))
    (is (= (tst \f) 3))
    (is (= (tst \b) -1))))
