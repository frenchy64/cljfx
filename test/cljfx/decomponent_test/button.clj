(ns cljfx.decomponent-test.button
  (:require [cljfx.api :as fx]))

;; Utils

(defn- sub-in [context ks]
  {:pre [((every-pred vector? seq) ks)]}
  (get-in (fx/sub context (first ks)) (rest ks)))

(defn get-clicked [context root]
  {:pre [(vector? root)]}
  (or (fx/sub context sub-in (conj root ::clicked))
      0))

;; Views

(defn view
  "Main view to show a button with internal state."
  [{:keys [fx/context fx/root] :as m}]
  (let [clicked (fx/sub context get-clicked root)]
    {:fx/type :button
     ; :fx/root implicitly passed to events
     :on-action {:event/type ::clicked
                 :clicked clicked}
     :text (str "Clicked x" clicked)}))

(def decomponent
  {:effects {::log-click (fn [{:keys [clicked fx/root]} dispatch!]
                           {:pre [root]}
                           (prn root "clicked!" clicked))}
   :event-handler-map {::clicked
                       (fn [{:keys [fx/context fx/root clicked] :as m}]
                         {:pre [root]}
                         {:context (fx/swap-context context update-in 
                                                    (conj root ::clicked)
                                                    (fnil inc 0))
                          ; Note: :fx/root must be passed manually to effects
                          ::log-click (select-keys m [:clicked :fx/root])})}})
