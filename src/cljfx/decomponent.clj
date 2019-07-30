(ns cljfx.decomponent
  "Part of a public API"
  (:require [cljfx.context :as context]
            [clojure.set :as set])
  (:import [clojure.lang MultiFn]))

(defn combine-event-handler
  "Create an event handler function by combining the given
  handler with decomponent event handler map.

  If handler is a multimethod, installs decomponent handlers
  in that multimethod and returns handler."
  [handler event-handler-map]
  (if (instance? MultiFn handler)
    (do (doseq [[k f] event-handler-map]
          (.addMethod ^MultiFn handler k f))
        handler)
    (if (empty? event-handler-map)
      handler
      (if handler
        #((get event-handler-map (:event/type %) handler) %)
        #(when-let [e (find event-handler-map (:event/type %))]
           ((val e) %))))))

(defn combine-render-error-handler
  "Create a cljfx renderer :error-handler given a handler intended for
  the :error-handler of a cljfx renderer, an atom containing
  the current app's context, and a set of functions that are called
  on a render error with the current app state, the thrown exception,
  and return the new app state."
  [handler *context swap-state-on-render-error-fns]
  (if (seq swap-state-on-render-error-fns)
    (let [handlers (fn [e]
                     ;TODO can this be generalized to simulate an "event" that returns
                     ; an effects map?
                     (swap! *context context/swap
                            (fn [state]
                              (reduce #(%2 %1 e) state swap-state-on-render-error-fns))))]
      (if handler
        (fn [e]
          (handler e)
          (handlers e))
        handlers))
    handler))

(defn resolve-decomponents
  "[(Set QualSym) -> ResolvedDecomponent]
  Takes a set of fully-qualified symbols that name vars containing `Decomponent`s.
  Returns a `ResolvedDecomponent`.

  (defalias Decomponent
    (HMap :optional {:effects EffectsMap
                     :co-effects CoEffectsMap
                     :event-handler-map (Map EventType EventHandler)
                     :decomponents (Set QualSym)
                     :swap-state-on-render-error [State Throwable -> State]}))

  (defalias ResolvedDecomponent
    (HMap :optional {:effects EffectsMap
                     :co-effects CoEffectsMap
                     :event-handler-map (Map EventType EventHandler)
                     :swap-state-on-render-error-fns (Set [State Throwable -> State])}))

  Return a combined decomponent with no :decomponents from a set of
  fully qualified symbols."
  [decomponents]
  (loop [to-process (set decomponents)
         processed #{}
         out {}]
    (if (empty? to-process)
      out
      (let [{depends :decomponents
             :keys [effects co-effects event-handler-map
                    swap-state-on-render-error]} @(resolve (first to-process))
            dependencies (set/difference (set depends) processed)]
        (recur (-> to-process
                   (disj (first to-process))
                   (into dependencies))
               (conj processed (first to-process))
               (cond-> out
                 effects (update :effects merge effects)
                 co-effects (update :co-effects merge co-effects)
                 event-handler-map (update :event-handler-map merge event-handler-map)
                 swap-state-on-render-error (update :swap-state-on-render-error-fns (fnil conj #{})
                                                    swap-state-on-render-error)))))))
