; Author: Ambrose Bonnaire-Sergeant
(ns e31-indefinite-transitions
  (:require [cljfx.api :as fx]))

(set! *warn-on-reflection* true)

(defn init-state []
  {})

(declare *state renderer)

(when (and (.hasRoot #'*state)
           (.hasRoot #'renderer))
  (fx/unmount-renderer *state renderer)
  (reset! *state (init-state)))

(def *state
  (atom (init-state)))

(defn- let-refs [refs desc]
  {:fx/type fx/ext-let-refs
   :refs refs
   :desc desc})

(defn- get-ref [ref]
  {:fx/type fx/ext-get-ref
   :ref ref})

#_
(defalias TransitionFn
  "A transition function takes a description of a node to transition,
   and an optional :status keyword argument to start the transition."
  [Desc & :optional {:status AnimationStatusProp} -> Desc])

#_
(ann apply-transition ['{:desc Desc, :transition-fn TransitionFn} -> Desc])
(defn apply-transition
  "Given a description and a transition function, starts the transition
  on the given description and returns the description."
  [{:keys [desc transition-fn]}]
  (let-refs {:node desc}
    (let-refs {:transition (transition-fn (get-ref :node) :status :running)}
      (get-ref :node))))

#_
(ann path-transition [PathTransitionProps -> TransitionFn])
;TODO demo
(defn path-transition [props]
  (fn [node & {:keys [status]}]
    {:pre [node]}
    (merge
      props
      {:fx/type :path-transition
       :node node
       ;:path {:fx/type :circle
       ;       :radius 10}
       ;:duration [1 :s]
       ;:orientation :orthogonal-to-tanget
       ;:cycle-count :indefinite
       }
      (when status
        {:status status}))))

#_
(ann fade-transition [FadeTransitionProps -> TransitionFn])
(defn fade-transition [props]
  (fn [node & {:keys [status]}]
    (merge 
      props
      {:fx/type :fade-transition
       :node node}
      (when status
        {:status status}))))

#_
(ann fill-transition [FillTransitionProps -> TransitionFn])
(defn fill-transition [props]
  (fn [shape & {:keys [status]}]
    (merge
      props
      {:fx/type :fill-transition
       :shape shape}
      (when status
        {:status status}))))

#_
(ann scale-transition [ScaleTransitionProps -> TransitionFn])
(defn scale-transition [props]
  (fn [node & {:keys [status]}]
    (merge
      props
      {:fx/type :scale-transition
       :node node}
      (when status
        {:status status}))))

#_
(ann stroke-transition [StrokeTransitionProps -> TransitionFn])
(defn stroke-transition [props]
  (fn [node & {:keys [status]}]
    (merge
      props
      {:fx/type :stroke-transition
       :shape node}
      (when status
        {:status status}))))

#_
(ann rotate-transition [RotateTransitionProps -> TransitionFn])
(defn rotate-transition [props]
  (fn [node & {:keys [status]}]
    (merge
      props
      {:fx/type :rotate-transition
       :node node}
      (when status
        {:status status}))))

#_
(ann pause-transition [PauseTransitionProps -> TransitionFn])
;TODO demo
(defn pause-transition [props]
  (fn [node & {:keys [status]}]
    (merge
      props
      {:fx/type :pause-transition
       ;:duration [0.4 :s]
       }
      (when status
        {:status status}))))

#_
(ann translate-transition [TranslateTransitionProps -> TransitionFn])
(defn translate-transition [props]
  (fn [node & {:keys [status]}]
    (merge
      props
      {:fx/type :translate-transition
       :node node}
      (when status
        {:status status}))))

#_
(defalias CompositeTransitionType
  (U ':parallel-transition
     ':sequential-transition))

#_
(ann composite-transition [CompositeTransitionType (Coll TransitionFn) -> TransitionFn])
(defn composite-transition [transition-type transition-fns]
  {:pre [(#{:parallel-transition :sequential-transition} transition-type)]}
  (fn [node & {:keys [status]}]
    {:pre [node]}
    (merge
      {:fx/type transition-type
       :node node
       :rate 1.0
       :auto-reverse true
       :cycle-count :indefinite
       :children (mapv #(% node) transition-fns)}
      (when status
        {:status status}))))

(defn on-grid [{:keys [rows columns children]}]
  {:pre [(vector? children)
         (<= (count children)
             (* rows columns))]}
  {:fx/type :grid-pane
   ;:hgap 20
   ;:vgap 20
   ;:padding 30
   :row-constraints (repeat rows {:fx/type :row-constraints
                                  :percent-height (/ 100 rows)})
   :column-constraints (repeat columns {:fx/type :column-constraints
                                        :percent-width (/ 100 columns)})
   :children (let [loc (atom -1)]
               (vec
                 (for [row (range rows)
                       col (range columns)
                       :let [_ (swap! loc inc)]
                       :when (< @loc (count children))]
                   (-> (nth children @loc)
                       (assoc :grid-pane/row row
                              :grid-pane/column col)))))})

(defn with-header [text desc]
  {:fx/type :grid-pane
   :row-constraints (repeat 1 {:fx/type :row-constraints
                               :percent-height 100/1})
   :column-constraints (repeat 2 {:fx/type :column-constraints
                                  :percent-width 100/2})
   :children [{:fx/type :label
               :grid-pane/row 0
               :grid-pane/column 0
               :text text}
              (assoc desc
                     :grid-pane/row 0
                     :grid-pane/column 1)]})



(defn view [{{:keys [timer-duration] :or {timer-duration 0}} :state}]
  (let [rect {:fx/type :rectangle
              :width 20
              :height 50}]
    {:fx/type :stage
     :showing true
     :always-on-top true
     :width 600
     :height 500
     :scene {:fx/type :scene
             :root {:fx/type on-grid
                    :rows 3
                    :columns 3
                    :children (mapv 
                                (fn [[header transition-fn]]
                                  (with-header
                                    header
                                    {:fx/type apply-transition
                                     :transition-fn transition-fn
                                     :desc rect}))
                                [["Fade" (fade-transition
                                           {:from-value 1.0
                                            :to-value 0.3
                                            :cycle-count :indefinite
                                            :duration [1 :s]
                                            :auto-reverse true})]
                                 ["Fill" (fill-transition
                                           {:from-value :green
                                            :to-value :red
                                            :cycle-count :indefinite
                                            :duration [1 :s]
                                            :auto-reverse true})]
                                 ["Scale" (scale-transition
                                            {:by-x 0.2
                                             :by-y 1.2
                                             :cycle-count :indefinite
                                             :duration [1 :s]
                                             :auto-reverse true})]
                                 ["Stroke" (stroke-transition
                                             {:from-value :red
                                              :to-value :yellow
                                              :cycle-count :indefinite
                                              :duration [0.5 :s]
                                              :auto-reverse true})]
                                 ["Rotate" (rotate-transition
                                             {:duration [0.5 :s]
                                              :from-angle 0
                                              :to-angle 185
                                              :cycle-count :indefinite
                                              :auto-reverse true
                                              :interpolator :ease-both})]
                                 ["Translate" (translate-transition
                                                {:duration [0.5 :s]
                                                 :from-x 0
                                                 :to-x 75
                                                 :interpolator :ease-both
                                                 :auto-reverse true
                                                 :cycle-count :indefinite})]
                                 ["Parallel" (composite-transition
                                               :parallel-transition
                                               [(rotate-transition
                                                  {:duration [0.5 :s]
                                                   :from-angle 0
                                                   :to-angle 185
                                                   :interpolator :ease-both})
                                                (translate-transition
                                                  {:duration [0.5 :s]
                                                   :from-x 0
                                                   :to-x 75
                                                   :interpolator :ease-both})])]
                                 ["Sequential" (composite-transition
                                                 :sequential-transition
                                                 [(rotate-transition
                                                    {:duration [0.5 :s]
                                                     :from-angle 0
                                                     :to-angle 185
                                                     :interpolator :ease-both})
                                                  (translate-transition
                                                    {:duration [0.5 :s]
                                                     :from-x 0
                                                     :to-x 75
                                                     :interpolator :ease-both})])]
                                 ["Nested" (composite-transition
                                             :sequential-transition
                                             [#_(composite-transition
                                                :parallel-transition
                                                [(pause-transition
                                                   {:duration [1 :s]})
                                                 ])
                                              (rotate-transition
                                                {:duration [0.5 :s]
                                                 :from-angle 0
                                                 :to-angle 185
                                                 :interpolator :ease-both})
                                              (translate-transition
                                                {:duration [0.5 :s]
                                                 :from-x 0
                                                 :to-x 75
                                                 :interpolator :ease-both})])]
                                 ])
                               }}}))

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type view
                                     :state state}))))

(fx/mount-renderer *state renderer)
