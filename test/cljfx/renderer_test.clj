(ns cljfx.renderer-test
  (:require [clojure.test :refer :all]
            [testit.core :refer :all]
            [cljfx.api :as fx]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [com.gfredericks.test.chuck.properties :as prop']
            )
  (:import [javafx.stage Stage]
           [javafx.scene.control Button]
           [javafx.scene.control TextField]
           [javafx.scene.input KeyCode KeyEvent]
           [java.util UUID]))

(set! *warn-on-reflection* true)

(deftest renderer-without-atom
  ; A renderer can be used without being mounted to an atom.
  ; Create a simple scene with a button and press it a
  ; few times, checking the presses were renderered.
  (let [; setup
        renderer (fx/create-renderer)
        pressed-atom (atom 0)
        stage-atom (atom nil)
        ;  /-----\
        ;  |x    |
        ;  |-----|
        ;  |click|
        ;  \-----/
        root (fn root [{:keys [pressed]}]
               (when pressed
                 (swap! pressed-atom inc))
               {:fx/type fx/ext-on-instance-lifecycle
                :on-created #(reset! stage-atom %)
                :desc
                {:fx/type :stage
                 :scene {:fx/type :scene
                         :root {:fx/type :button
                                :text "click"
                                :on-action (fn [_]
                                             (renderer {:fx/type root
                                                        :pressed true}))}}}})
        check-presses (fn [presses]
                        (let [_ @(renderer)
                              _ (Thread/sleep 100)]
                          (is (= @pressed-atom presses)
                              "Number of button presses should be consistent")))
        click-button (fn []
                       (let [^Stage stage @stage-atom
                             _ (assert (instance? Stage stage))
                             ^Button button (-> stage .getScene .getRoot)]
                         (.fire button)))
        _ (check-presses 0)
        ; start app
        _ @(renderer {:fx/type root})
        _ (check-presses 0)
        ; first press
        _ (click-button)
        _ (check-presses 1)
        ; second press
        _ (click-button)
        _ (check-presses 2)
        ; close app
        _ @(renderer nil)
        ]
    ))

(deftest renderer-with-state-atom
  ; A renderer can be mounted to a state atom referencing raw data.
  ; This scene syncs the stage title with a text-field and the state atom,
  ; and updates the title by manually setting the text field.
  (let [; setup
        *state (atom {:title "1"})
        stage-atom (atom nil)
        ;  /------------\
        ;  |x   Title   |
        ;  |------------|
        ;  |Title       |
        ;  \------------/
        root (fn [{:keys [title]}]
               {:fx/type fx/ext-on-instance-lifecycle
                :on-created #(reset! stage-atom %)
                :desc
                {:fx/type :stage
                 :title title
                 :scene {:fx/type :scene
                         :root {:fx/type :text-field
                                :on-text-changed #(swap! *state assoc :title %)
                                :text title}}}})
        renderer (fx/create-renderer
                   :middleware (fx/wrap-map-desc assoc :fx/type root))
        update-title (fn [title]
                       (let [^Stage stage @stage-atom
                             ^TextField text-field (-> stage .getScene .getRoot)]
                         (.setText text-field title)))
        check-title (fn [title]
                      (let [_ @(renderer)
                            _ (Thread/sleep 100)
                            ^Stage stage @stage-atom
                            ^TextField text-field (-> stage .getScene .getRoot)]
                        (is (= title
                               (:title @*state)
                               (.getTitle stage)
                               (.getText text-field))
                            "Title should be consistent in *state, stage and text-field")))
        ; start app
        _ @(fx/mount-renderer *state renderer)
        ; initial title is "1"
        _ (check-title "1")
        ; update title to "2"
        _ (update-title "2")
        _ (check-title "2")
        ; revert title to "1"
        _ (update-title "1")
        _ (check-title "1")
        ; close app
        _ @(fx/unmount-renderer *state renderer)
        ]
  ))

(deftest renderer-with-state-atom-and-map-desc
  ; A todo app that uses a map event handler mounted on a raw state atom.
  (let [; setup
        gen-id #(str (UUID/randomUUID))
        *state (atom {:typed-text ""
                      :by-id (into {}
                                   (map (fn [m]
                                          ((juxt identity #(assoc m :id %))
                                           (gen-id))))
                                   [{:text "Buy milk"
                                     :done true}
                                    {:text "Buy socks"
                                     :done false}])})
        todo-view (fn [{:keys [text id done]}]
                    {:fx/type :h-box
                     :spacing 5
                     :padding 5
                     :children [{:fx/type :check-box
                                 :selected done
                                 :on-selected-changed {:event/type ::set-done :id id}}
                                {:fx/type :label
                                 :style {:-fx-text-fill (if done :grey :black)}
                                 :text text}]})
        stage-atom (atom nil)
        v-box-atom (atom nil)
        text-field-atom (atom nil)
        root (fn [{:keys [by-id typed-text]}]
               {:fx/type fx/ext-on-instance-lifecycle
                :on-created #(reset! stage-atom %)
                :desc
                {:fx/type :stage
                 :showing true
                 :always-on-top true
                 :scene {:fx/type :scene
                         :root {:fx/type :v-box
                                :pref-width 300
                                :pref-height 400
                                :children [{:fx/type :scroll-pane
                                            :v-box/vgrow :always
                                            :fit-to-width true
                                            :content 
                                            {:fx/type fx/ext-on-instance-lifecycle
                                             :on-created #(reset! v-box-atom %)
                                             :desc
                                             {:fx/type :v-box
                                              :id "v-box"
                                              :children (->> by-id
                                                             vals
                                                             (sort-by (juxt :done :id))
                                                             (map #(assoc %
                                                                          :fx/type todo-view
                                                                          :fx/key (:id %))))}}}
                                           {:fx/type fx/ext-on-instance-lifecycle
                                            :v-box/margin 5
                                            :on-created #(reset! text-field-atom %)
                                            :desc
                                            {:fx/type :text-field
                                             :id "::text-field"
                                             :text typed-text
                                             :prompt-text "Add new todo and press ENTER"
                                             :on-text-changed {:event/type ::type
                                                               }
                                             :on-key-pressed {:event/type ::press
                                                              }}}]}}}})
        map-event-handler (fn [event]
                            (case (:event/type event)
                              ::set-done (swap! *state assoc-in [:by-id (:id event) :done] (:fx/event event))
                              ::type (swap! *state assoc :typed-text (:fx/event event))
                              ::press (when (= KeyCode/ENTER (.getCode ^KeyEvent (:fx/event event)))
                                        (swap! *state #(-> %
                                                           (assoc :typed-text "")
                                                           (assoc-in [:by-id (count (:by-id %))]
                                                                     {:id (count (:by-id %))
                                                                      :text (:typed-text %)
                                                                      :done false}))))
                              nil))
        renderer (fx/create-renderer
                   :middleware (fx/wrap-map-desc assoc :fx/type root)
                   :opts {:fx.opt/map-event-handler map-event-handler})
        change-typed-text-via-state (fn [typed-text] (swap! *state assoc :typed-text typed-text))
        change-typed-text-via-text-field (fn [typed-text]
                                           @(fx/on-fx-thread
                                              (.setText ^TextField @text-field-atom typed-text)))
        check-typed-text (fn [typed-text]
                           (let [state (:typed-text @*state)
                                 field (.getText ^TextField @text-field-atom)]
                             (is (= typed-text
                                    state
                                    field)
                                 "Typed text should be consistent between state and rendering")))

        ; start app
        _ @(fx/mount-renderer *state renderer)
        res (checking "Can change either the atom or text-field" 1000
                      [typed-text gen/string
                       change-typed-text (gen/elements
                                           [change-typed-text-via-state
                                            change-typed-text-via-text-field])]
                      (run! #(% typed-text)
                            [change-typed-text
                             check-typed-text]))
        ;_ (prn "state" @*state)
        ;_ @(fx/unmount-renderer *state renderer)
        ]
    :ok))
