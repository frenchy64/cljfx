(ns cljfx.event-handler
  "Part of a public API")

(defn make-deref-co-effect [*ref]
  #(deref *ref))

(defn make-deref-co-effect-with-root [*context]
  (->
    (fn [event]
      (prn "deref-co-effect" (find event :fx/root))
      (-> (deref *context)
          (assoc :cljfx.context/root (:fx/root event []))))
    (with-meta {::provide-co-effect-event true})))

(defn wrap-co-effects [f co-effect-id->producer]
  (fn [event]
    (f (reduce (fn [acc [k v]]
                 (assoc acc k (if (-> v meta ::provide-co-effect-event)
                                (v event)
                                (v))))
               event
               co-effect-id->producer))))

(defn make-reset-effect [*atom]
  (fn [v _]
    (reset! *atom v)))

(defn make-reset-effect-without-root [*context]
  (fn [v _]
    (reset! *context (dissoc v :cljfx.context/root))))

(defn dispatch-effect [v dispatch!]
  (dispatch! v))

(defn wrap-effects [f effect-id->consumer]
  (fn dispatch-sync!
    ([event]
     (dispatch-sync! event dispatch-sync!))
    ([event dispatch!]
     (doseq [[fx-effect value] (f event)]
       ((effect-id->consumer fx-effect) value
        #(dispatch!
           (cond-> %
             (and (map? %)
                  (contains? % :event/type))
             (update :fx/root
                     (fn [explicit-root]
                       (or explicit-root (:fx/root event)))))))))))

(defn- process-event [_ f e dispatch-async! *maybe-promise]
  (f e dispatch-async!)
  (when *maybe-promise
    (deliver *maybe-promise nil)))

(defn wrap-async [f agent-options]
  (let [*agent (apply agent nil (mapcat identity agent-options))]
    (fn dispatch-async! [event]
      (if (:fx/sync event)
        (let [*promise (promise)]
          (send *agent process-event f event dispatch-async! *promise)
          @*promise)
        (send *agent process-event f event dispatch-async! nil))
      nil)))
