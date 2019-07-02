(ns cljfx.ext.node
  (:require [cljfx.lifecycle :as lifecycle]
            [cljfx.mutator :as mutator]
            [cljfx.prop :as prop]
            [cljfx.coerce :as coerce])
  (:import [javafx.scene.control Tooltip]
           [javafx.scene Node]
           [javafx.event Event EventHandler]))

(def with-tooltip-props
  "Extension lifecycle providing `:tooltip` prop to any Node

  Note that Controls already have `:tooltip` property, so this is useful only for other
  types of Nodes

  Supported keys:
  - `:desc` (required) - component description of a Node
  - `:props` (optional) - map of tooltip-related props:
    - `:tooltip` - description of a tooltip"
  (lifecycle/make-ext-with-props
    lifecycle/dynamic
    {:tooltip (prop/make
                (mutator/adder-remover
                  #(Tooltip/install %1 %2)
                  #(Tooltip/uninstall %1 %2))
                lifecycle/dynamic)}))

(def with-event-filter-props
  "Extension lifecycle providing `:event-filter` prop to any Node
  for attaching event filters declaratively.

  Supported keys:
  - `:desc` (required) - component description of a Node
  - `:props` (optional) - map of event-filter-related props:
    - `:event-filter` - description of an event
  
  Example:
    {:fx/type fx.ext.node/with-event-filter-props
     :props {:event-filter {:event/type ::handle-event-filter}}
     :desc {:fx/type root-node}}"
  (lifecycle/make-ext-with-props
    lifecycle/dynamic
    {:event-filter (prop/make
                     (mutator/adder-remover
                       #(.addEventFilter ^Node %1 Event/ANY ^EventHandler %2)
                       #(.removeEventFilter ^Node %1 Event/ANY ^EventHandler %2))
                     lifecycle/event-handler
                     :coerce coerce/event-handler)}))
