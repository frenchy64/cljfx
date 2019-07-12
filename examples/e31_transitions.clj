; Author: Ambrose Bonnaire-Sergeant
(ns e31-transitions
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

(defn animate-entrance-desc [{:keys [desc status] :or {status :running}}]
  {:pre [desc]}
  (let-refs {:node desc}
    (let-refs {:animation1
               {:fx/type :parallel-transition
                :node (get-ref :node)
                :auto-reverse true
                :cycle-count :indefinite
                :status status
                :children [; rotate 180 degrees
                           {:fx/type :rotate-transition
                            :node (get-ref :node)
                            :duration [0.5 :s]
                            :from-angle 0
                            :to-angle 185
                            :interpolator :ease-both}
                           ; while also moving left to right
                           {:fx/type :translate-transition
                            :node (get-ref :node)
                            :duration [0.5 :s]
                            :from-x 0
                            :to-x 75
                            :interpolator :ease-both}]}}
      (get-ref :node))))

(defn path-transition [{:keys [desc]}]
  (let-refs {:node desc}
    (let-refs {:path {:fx/type :path-transition
                      :node (get-ref :node)
                      :path {:fx/type :circle
                             :radius 35}
                      :duration [1 :s]
                      :orientation :orthogonal-to-tanget
                      :cycle-count :indefinite
                      :status :running}}
      (get-ref :node))))

(defn fade-transition [{:keys [desc]}]
  (let-refs {:node desc}
    (let-refs {:fade {:fx/type :fade-transition
                      :node (get-ref :node)
                      :from-value 1.0
                      :to-value 0.3
                      :cycle-count :indefinite
                      :duration [1 :s]
                      :auto-reverse true
                      :status :running}}
      (get-ref :node))))

(defn fill-transition [{:keys [desc]}]
  (let-refs {:shape desc}
    (let-refs {:fade {:fx/type :fill-transition
                      :shape (get-ref :shape)
                      :from-value :green
                      :to-value :red
                      :cycle-count :indefinite
                      :duration [1 :s]
                      :auto-reverse true
                      :status :running}}
      (get-ref :shape))))

(defn scale-transition [{:keys [desc]}]
  (let-refs {:node desc}
    (let-refs {:fade {:fx/type :scale-transition
                      :node (get-ref :node)
                      :by-x 1
                      :by-y 5
                      :cycle-count :indefinite
                      :duration [1 :s]
                      :auto-reverse true
                      :status :running}}
      (get-ref :node))))

(defn stroke-transition [{:keys [desc]}]
  (let-refs {:shape desc}
    (let-refs {:fade {:fx/type :stroke-transition
                      :shape (get-ref :shape)
                      :from-value :red
                      :to-value :yellow
                      :cycle-count :indefinite
                      :duration [0.5 :s]
                      :auto-reverse true
                      :status :running}}
      (get-ref :shape))))

(defn with-header [text row col desc]
  {:fx/type :v-box
   :fill-width true
   :grid-pane/row row
   :grid-pane/column col
   :spacing 10
   :children [{:fx/type :label
               :text text}
              desc]})

(defn view [{{:keys [timer-duration] :or {timer-duration 0}} :state}]
  {:fx/type :stage
   :showing true
   :always-on-top true
   :width 600
   :height 600
   :scene {:fx/type :scene
           :root {:fx/type :grid-pane
                  :row-constraints (repeat 4 {:fx/type :row-constraints
                                              :percent-height 100/4})
                  :column-constraints (repeat 2 {:fx/type :column-constraints
                                                 :percent-width 100/3})
                  :children [(with-header
                               "Parallel rotate+translate"
                               0 0
                               {:fx/type animate-entrance-desc
                                :desc {:fx/type :rectangle
                                       :width 50
                                       :height 100}})
                             (with-header
                               "Path transition"
                               1 0
                               {:fx/type path-transition
                                :desc {:fx/type :rectangle
                                       :width 50
                                       :height 100}})
                             (with-header
                               "Fade transition"
                               2 0
                               {:fx/type fade-transition
                                :desc {:fx/type :rectangle
                                       :width 50
                                       :height 100}})
                             (with-header
                               "Fill transition"
                               3 0
                               {:fx/type fill-transition
                                :desc {:fx/type :rectangle
                                       :width 50
                                       :height 100}})
                             (with-header
                               "Scale transition"
                               0 1
                               {:fx/type scale-transition
                                :desc {:fx/type :rectangle
                                       :width 50
                                       :height 15}})
                             (with-header
                               "Stroke transition"
                               1 1
                               {:fx/type stroke-transition
                                :desc {:fx/type :rectangle
                                       :stroke-width 10
                                       :arc-height 20
                                       :arc-width 20
                                       :width 50
                                       :height 100}})
                             (with-header
                               "Parallel rotate+translate"
                               2 1
                               (let [rect (keyword (gensym))]
                                 {:fx/type animate-entrance-desc
                                  :desc {:fx/type :rectangle
                                         :width 50
                                         :height 100}}))]}}})

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type view
                                     :state state}))))

(fx/mount-renderer *state renderer)
