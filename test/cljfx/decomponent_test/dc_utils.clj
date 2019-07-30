(ns cljfx.decomponent-test.dc-utils
  (:require [cljfx.api :as fx]))

(defn- dissoc-in [m path]
  (if-some [[f & n] (seq path)]
    (if n
      (cond-> m
        (contains? m f) (update f dissoc-in n))
      (dissoc m f))
    m))

(def decomponent
  {:event-handler-map {::delete-decomponent
                       (fn [{:keys [fx/context path] :as m}]
                         {:pre [path]}
                         (prn "::delete-decomponent" path)
                         {:context (fx/swap-context context dissoc-in path)})}})
