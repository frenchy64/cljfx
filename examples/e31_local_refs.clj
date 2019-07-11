(ns e31-local-refs
  (:require [cljfx.api :as fx]))

; # Problem
; 
; fx/ext-let-refs is designed to provide dynamic scoping, and is the only
; means of binding refs in cljfx.
; Lexically scoped refs is sometimes desirable its not immediately obvious
; how to implement them correctly and performantly.

; # Proposed solution
;
; Provide a simple macro `fx/local-refs` for lexically-scoped ref-keys.
; The idea is to generate ref keys at compile-time so then cljfx's caches
; are still effective even when passing these keys to other functions.
; 
; The code
;
;   (fx/local-refs [key]
;     {:fx/type fx/ext-let-refs
;      :refs {key ...}
;      :desc {... {:fx/type fx/ext-get-ref
;                  :ref key}}})
;
; expands to something similar to 
;
;   (let [key (gensym 'key)]
;     {:fx/type fx/ext-let-refs
;      :refs {key ...}
;      :desc {... {:fx/type fx/ext-get-ref
;                  :ref key}}})
;
; except the `gensym` is done at compile-time.
;
; See `view` below for a demo. Run this file and follow the instructions
; in the window to verify this approach is performant.

(declare with-vheader with-hheader on-grid record-render)

(defmacro gen-render-key []
  (list 'quote (gensym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start interesting stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn juxtapose-shapes [{:keys [fx/context desc render-key flip]}]
  {:pre [(boolean? flip)
         render-key]}
  (let [nrender (record-render render-key)]
    ; lexically scope `circle-ref` to avoid unintended
    ; capture of `fx/ext-get-ref`s in `desc`
    (fx/local-refs [circle-ref]
      {:fx/type fx/ext-let-refs
       :refs {circle-ref {:fx/type :circle :radius 10}}
       :desc {:fx/type :h-box
              :children (concat
                          ((if flip identity rseq)
                           [(with-vheader "the-circle" {:fx/type fx/ext-get-ref
                                                        :ref circle-ref})
                            (with-vheader "the-rect" desc)])
                          [{:fx/type :label
                            :text (str "Renderings: " nrender)}])}})))

(defn view [{:keys [fx/context]}]
  (let [flip (fx/sub context :flip)
        rect-desc {:fx/type :rectangle :width 50 :height 100}]
    {:fx/type :stage
     :showing true
     :always-on-top true
     :width 800
     :height 700
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :children
                    [{:fx/type :button
                      :on-action {:event/type ::flip}
                      :text "Flip"}
                     {:fx/type :label
                      :wrap-text true
                      :text (str "Press the above button to swap the shapes. The number of rerenderings\n"
                                 "is recorded to the right of each set of shapes. Notice the gensym version\n"
                                 "always rerenders, but the equally hygienic fx/local-refs version only\n"
                                 "renders twice.")}
                     {:fx/type on-grid
                      :rows 2
                      :columns 2
                      :children [(with-vheader
                                   "fx/local-refs (hygienic)"
                                   (fx/local-refs
                                     [rect-key]
                                     {:fx/type fx/ext-let-refs
                                      :refs {rect-key rect-desc}
                                      ; `rect-key` cannot be captured by `juxtapose-shapes`,
                                      ; and rerenderings are efficient as `:desc` is the same
                                      ; on each call.
                                      :desc {:fx/type juxtapose-shapes
                                             :flip flip
                                             :render-key (gen-render-key)
                                             :desc {:fx/type fx/ext-get-ref
                                                    :ref rect-key}}}))
                                 (with-vheader
                                   "clojure.core/let (unhygienic)"
                                   (let [rect-key :rect]
                                     {:fx/type fx/ext-let-refs
                                      :refs {rect-key rect-desc}
                                      ; `juxtapose-shapes` could capture `rect-key`,
                                      ; but rerenderings are efficient as `:desc`
                                      ; is the same on each call.
                                      :desc {:fx/type juxtapose-shapes
                                             :flip flip
                                             :render-key (gen-render-key)
                                             :desc {:fx/type fx/ext-get-ref
                                                    :ref rect-key}}}))
                                 (with-vheader
                                   "clojure.core/let (hygienic, but rerenders)"
                                   (let [rect-key (gensym 'rect)]
                                     {:fx/type fx/ext-let-refs
                                      :refs {rect-key rect-desc}
                                      ; `juxtapose-shapes` cannot accidentally capture `rect-key`,
                                      ; but this triggers unnecessary rerenderings as
                                      ; the `:desc` argument below be different on every call.
                                      :desc {:fx/type juxtapose-shapes
                                             :flip flip
                                             :render-key (gen-render-key)
                                             :desc {:fx/type fx/ext-get-ref
                                                    :ref rect-key}}}))]}]}}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End interesting stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *dev (atom {}))
(defn record-render [k]
  (swap! *dev update-in [:renderings k] (fnil inc 0))
  (get-in @*dev [:renderings k]))

(defn init-state []
  {:flip false})

(declare *context app)

(when (and (.hasRoot #'*context)
           (.hasRoot #'app))
  (fx/unmount-renderer *context (:renderer app))
  (reset! *context (fx/create-context (init-state))))

(def *context
  (atom (fx/create-context (init-state))))

(defn on-grid [{:keys [rows columns children]}]
  {:pre [(vector? children)
         (<= (count children)
             (* rows columns))]}
  {:fx/type :grid-pane
   :hgap 20
   :vgap 20
   :padding 30
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

(defn with-vheader [text desc]
  {:fx/type :v-box
   :fill-width true
   :spacing 20
   :padding 10
   :children [{:fx/type :label
               :wrap-text true
               :text text}
              desc]})

(defn with-hheader [text desc]
  {:fx/type :h-box
   :fill-height true
   :spacing 10
   :children [{:fx/type :label
               :wrap-text true
               :text text}
              desc]})

(defn handle-event [{etype :event/type :keys [fx/context fx/event]}]
  {:pre [context]}
  (prn "handle-event" etype)
  (case etype
    ::flip {:context (fx/swap-context context update :flip not)}))

(def app
  (fx/create-app *context
    :event-handler handle-event
    :desc-fn (fn [_]
               {:fx/type view})))
