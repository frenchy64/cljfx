(ns cljfx.tab-pane-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [cljfx.fx.tab-pane :as fx.tab-pane])
  (:import [java.util ArrayList UUID Collection]
           [javafx.scene.control Tab]
           [com.sun.javafx.scene.control TabObservableList]
           ))

(defn ^TabObservableList create-tab-observable-list [^Collection coll]
  (TabObservableList. (ArrayList. coll)))

(defn create-tab []
  (doto (Tab.)
    (.setId (str (UUID/randomUUID)))))

(deftest tab-observable-list-test
  (let [init-tabs (apply list (repeatedly 10 create-tab))
        tol (create-tab-observable-list init-tabs)]
    (fx.tab-pane/sync-tab-observable-list tol (reverse init-tabs))))
