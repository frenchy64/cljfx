(ns cljfx.decomponent-test.button
  (:require [cljfx.api :as fx]))

;; Utils

(defn- sub-in [context ks]
  {:pre [((every-pred vector? seq) ks)]}
  (get-in (fx/sub context (first ks)) (rest ks)))

(defn get-clicked [context path]
  {:pre [(vector? path)]}
  (or (fx/sub context sub-in (conj path ::clicked))
      0))

;; Views

(defn button-with-state
  "Use this view to show a button"
  [{:keys [fx/context fx/path] :as m}]
  {:pre [path]}
  (let [clicked (fx/sub context get-clicked path)]
    {:fx/type :button
     :on-action {:event/type ::clicked
                 :clicked clicked}
     :text (str "Clicked x" clicked)}))

(def decomponent
  {:init-state {::button {}}
   :effects {::log-click (fn [{:keys [clicked fx/path]} dispatch!]
                           (prn path "clicked!" clicked))}
   :event-handler-map {::clicked
                       (fn [{:keys [fx/context fx/path clicked]}]
                         {:pre [path]}
                         (prn "button path" path)
                         {:context (fx/swap-context context update-in 
                                                    (conj path ::clicked)
                                                    (fnil inc 0))
                          ::log-click {:clicked clicked}})}})
