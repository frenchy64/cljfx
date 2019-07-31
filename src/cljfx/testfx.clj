(ns cljfx.testfx
  (:require [cljfx.api :as fx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [org.testfx.robot Motion]
           [javafx.geometry Point2D]
           [javafx.scene.input MouseButton KeyCode]))

(set! *warn-on-reflection* true)

(def ^:private ^Motion coerce-motion (coerce/enum Motion))

(def ^:private ^MouseButton coerce-mouse-button (coerce/enum javafx.scene.input.MouseButton))

(defn- coerce-point2d [p]
  (cond
    (instance? Point2D p) p
    (and (vector? p)
         (= 2 (count p))) (let [[x y] p]
                            (Point2D. x y))
    :else (coerce/fail Point2D p)))

(defn ^FxRobot robot []
  (FxRobot.))

(defn click-on [^FxRobot robot & {:keys [buttons motion bounds point-query point]
                                  :or {motion :default}}]
  {:pre [(#{0 1} (count (into []
                              (filter identity)
                              [point-query
                               point
                               bounds])))]}
  (let [motion (coerce-motion motion)
        buttons ^"[Ljavafx.scene.input.MouseButton;"
        (into-array javafx.scene.input.MouseButton (mapv coerce-mouse-button buttons))]
    (cond
      point (.clickOn robot (coerce-point2d point) motion buttons)
      point-query nil
      bounds nil
      :else nil)
    robot))
