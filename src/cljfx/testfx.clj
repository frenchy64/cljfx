(ns cljfx.testfx
  (:require [cljfx.api :as fx]
            [cljfx.component :as component]
            [cljfx.coerce :as coerce]
            [clojure.string :as str])
  (:import [org.testfx.api FxRobot FxRobotContext FxToolkit]
           [org.testfx.robot Motion BaseRobot]
           [javafx.geometry Point2D Bounds]
           [javafx.scene Node Scene]
           [javafx.scene.input MouseButton KeyCode]))

(set! *warn-on-reflection* true)

(def ^:private ^Motion coerce-motion (coerce/enum Motion))

(def ^:private ^MouseButton coerce-mouse-button (coerce/enum javafx.scene.input.MouseButton))

(defn- ^Point2D coerce-point2d [p]
  (cond
    (instance? Point2D p) p
    (map? p) (Point2D. (:x p) (:y p))
    (and (vector? p)
         (= 2 (count p))) (let [[x y] p]
                            (Point2D. x y))
    :else (coerce/fail Point2D p)))

(defn ^FxRobot robot []
  (FxRobot.))

(defn click-on [^FxRobot robot & {:keys [target buttons motion bounds point-query point]
                                  :or {motion :default}}]
  {:pre [(#{0 1} (count (into []
                              (filter identity)
                              [point-query
                               point
                               bounds])))]}
  (let [motion (coerce-motion motion)
        buttons ^"[Ljavafx.scene.input.MouseButton;"
        (into-array javafx.scene.input.MouseButton (mapv coerce-mouse-button buttons))]
    (cond
      (instance? Scene target) (.clickOn robot ^Scene target motion buttons)
      (instance? Node target) (.clickOn robot ^Node target motion buttons)
      (instance? Bounds target) (.clickOn robot ^Bounds target motion buttons)
      (instance? Point2D target) (.clickOn robot ^Point2D target motion buttons)
      ;(instance? PointQuery target) (.clickOn robot ^PointQuery target motion buttons)
      ;NYI
      (instance? String target) (.clickOn robot ^String target motion buttons)
      point-query nil
      bounds nil
      :else nil)
    robot))

; Primitives

(defn ^BaseRobot base-robot [^FxRobot robot]
  (-> robot .robotContext .getBaseRobot))

(defmulti exec :testfx/op)

;{:testfx/op :mouse/press
; :testfx/robot Robot
; :button Button}
(defmethod exec :mouse/press
  [{:keys [testfx/robot button]
    :or {button :primary}}]
  (.pressMouse (base-robot robot) (coerce-mouse-button button)))

;{:testfx/op :mouse/release 
; :button Button}
(defmethod exec :mouse/release
  [{:keys [testfx/robot button]
    :or {button :primary}}]
  (.releaseMouse (base-robot robot) (coerce-mouse-button button)))

;{:testfx/op :mouse/scroll
; :amount Int}
(defmethod exec :mouse/scroll
  [{:keys [testfx/robot amount]}]
  (.scrollMouse (base-robot robot) amount))

;{:testfx/op :mouse/move
; :point Point2D}
(defmethod exec :mouse/move
  [{:keys [testfx/robot point]}]
  (.moveMouse (base-robot robot) (coerce-point2d point)))

#_
{:testfx/op :assert
 :button Button}

#_
{:testfx/op :key/press
 :button Button}

#_
{:testfx/op :key/release
 :button Button}

#_
{:testfx/op :sleep 
 :ms Int}

#_
{:testfx/op :capture
 :region Rectangle2D
 :out Path}

#_
(deftest robot
  (is (-> robot
          (.lookup ".button")
          (.queryAs Button))))

#_
(query robot ".button")

(def keyword->class-sym
  '{:sphere javafx.scene.shape.Sphere,
    :indexed-cell javafx.scene.control.IndexedCell,
    :svg-path javafx.scene.shape.SVGPath,
    :path javafx.scene.shape.Path,
    :html-editor javafx.scene.web.HTMLEditor,
    :text-area javafx.scene.control.TextArea,
    :pie-chart javafx.scene.chart.PieChart,
    :parallel-camera javafx.scene.ParallelCamera,
    :tile-pane javafx.scene.layout.TilePane,
    :table-view javafx.scene.control.TableView,
    :bloom javafx.scene.effect.Bloom,
    :gaussian-blur javafx.scene.effect.GaussianBlur,
    :point-light javafx.scene.PointLight,
    :progress-bar javafx.scene.control.ProgressBar,
    :combo-box javafx.scene.control.ComboBox,
    :pane javafx.scene.layout.Pane,
    :popup javafx.stage.Popup,
    :menu-item javafx.scene.control.MenuItem,
    :anchor-pane javafx.scene.layout.AnchorPane,
    :custom-menu-item javafx.scene.control.CustomMenuItem,
    :group javafx.scene.Group,
    :media-view javafx.scene.media.MediaView,
    :stage javafx.stage.Stage,
    :arc javafx.scene.shape.Arc,
    :light-spot javafx.scene.effect.Light$Spot,
    :affine javafx.scene.transform.Affine,
    :shear javafx.scene.transform.Shear,
    :radio-menu-item javafx.scene.control.RadioMenuItem,
    :close-path javafx.scene.shape.ClosePath,
    :list-spinner-value-factory javafx.scene.control.SpinnerValueFactory$ListSpinnerValueFactory,
    :number-axis javafx.scene.chart.NumberAxis,
    :scale javafx.scene.transform.Scale,
    :button javafx.scene.control.Button,
    :lighting javafx.scene.effect.Lighting,
    :tool-bar javafx.scene.control.ToolBar,
    :bar-chart javafx.scene.chart.BarChart,
    :xy-chart-series javafx.scene.chart.XYChart$Series,
    :radio-button javafx.scene.control.RadioButton,
    :perspective-camera javafx.scene.PerspectiveCamera,
    :move-to javafx.scene.shape.MoveTo,
    :text-input-dialog javafx.scene.control.TextInputDialog,
    :split-menu-button javafx.scene.control.SplitMenuButton,
    :rotate-transition javafx.animation.RotateTransition,
    :pause-transition javafx.animation.PauseTransition,
    :grid-pane javafx.scene.layout.GridPane,
    :combo-box-list-cell javafx.scene.control.cell.ComboBoxListCell,
    :sub-scene javafx.scene.SubScene,
    :scale-transition javafx.animation.ScaleTransition,
    :text-flow javafx.scene.text.TextFlow,
    :box javafx.scene.shape.Box,
    :cubic-curve javafx.scene.shape.CubicCurve,
    :quad-curve-to javafx.scene.shape.QuadCurveTo,
    :circle javafx.scene.shape.Circle,
    :motion-blur javafx.scene.effect.MotionBlur,
    :line-to javafx.scene.shape.LineTo,
    :h-line-to javafx.scene.shape.HLineTo,
    :popup-control javafx.scene.control.PopupControl,
    :drop-shadow javafx.scene.effect.DropShadow,
    :flow-pane javafx.scene.layout.FlowPane,
    :stacked-bar-chart javafx.scene.chart.StackedBarChart,
    :alert javafx.scene.control.Alert,
    :mesh-view javafx.scene.shape.MeshView,
    :color-adjust javafx.scene.effect.ColorAdjust,
    :progress-indicator javafx.scene.control.ProgressIndicator,
    :text-field javafx.scene.control.TextField,
    :tree-view javafx.scene.control.TreeView,
    :menu-bar javafx.scene.control.MenuBar,
    :fill-transition javafx.animation.FillTransition,
    :color-picker javafx.scene.control.ColorPicker,
    :image-input javafx.scene.effect.ImageInput,
    :category-axis javafx.scene.chart.CategoryAxis,
    :stack-pane javafx.scene.layout.StackPane,
    :xy-chart-data javafx.scene.chart.XYChart$Data,
    :tree-item javafx.scene.control.TreeItem,
    :accordion javafx.scene.control.Accordion,
    :spinner javafx.scene.control.Spinner,
    :v-box javafx.scene.layout.VBox,
    :double-spinner-value-factory javafx.scene.control.SpinnerValueFactory$DoubleSpinnerValueFactory,
    :canvas javafx.scene.canvas.Canvas,
    :password-field javafx.scene.control.PasswordField,
    :v-line-to javafx.scene.shape.VLineTo,
    :region javafx.scene.layout.Region,
    :displacement-map javafx.scene.effect.DisplacementMap,
    :cubic-curve-to javafx.scene.shape.CubicCurveTo,
    :scroll-pane javafx.scene.control.ScrollPane,
    :triangle-mesh javafx.scene.shape.TriangleMesh,
    :split-pane javafx.scene.control.SplitPane,
    :toggle-button javafx.scene.control.ToggleButton,
    :date-picker javafx.scene.control.DatePicker,
    :table-column javafx.scene.control.TableColumn,
    :web-view javafx.scene.web.WebView,
    :line javafx.scene.shape.Line,
    :list-view javafx.scene.control.ListView,
    :choice-box javafx.scene.control.ChoiceBox,
    :bubble-chart javafx.scene.chart.BubbleChart,
    :stacked-area-chart javafx.scene.chart.StackedAreaChart,
    :pagination javafx.scene.control.Pagination,
    :media-player javafx.scene.media.MediaPlayer,
    :quad-curve javafx.scene.shape.QuadCurve,
    :ambient-light javafx.scene.AmbientLight,
    :check-box javafx.scene.control.CheckBox,
    :label javafx.scene.control.Label,
    :image-view javafx.scene.image.ImageView,
    :color-input javafx.scene.effect.ColorInput,
    :cylinder javafx.scene.shape.Cylinder,
    :border-pane javafx.scene.layout.BorderPane,
    :row-constraints javafx.scene.layout.RowConstraints,
    :context-menu javafx.scene.control.ContextMenu,
    :integer-spinner-value-factory javafx.scene.control.SpinnerValueFactory$IntegerSpinnerValueFactory,
    :translate-transition javafx.animation.TranslateTransition,
    :ellipse javafx.scene.shape.Ellipse,
    :pie-chart-data javafx.scene.chart.PieChart$Data,
    :inner-shadow javafx.scene.effect.InnerShadow,
    :light-distant javafx.scene.effect.Light$Distant,
    :stroke-transition javafx.animation.StrokeTransition,
    :translate javafx.scene.transform.Translate,
    :dialog javafx.scene.control.Dialog,
    :rotate javafx.scene.transform.Rotate,
    :sequential-transition javafx.animation.SequentialTransition,
    :hyperlink javafx.scene.control.Hyperlink,
    :tab-pane javafx.scene.control.TabPane,
    :column-constraints javafx.scene.layout.ColumnConstraints,
    :button-bar javafx.scene.control.ButtonBar,
    :blend javafx.scene.effect.Blend,
    :path-transition javafx.animation.PathTransition,
    :box-blur javafx.scene.effect.BoxBlur,
    :fade-transition javafx.animation.FadeTransition,
    :menu-button javafx.scene.control.MenuButton,
    :area-chart javafx.scene.chart.AreaChart,
    :choice-dialog javafx.scene.control.ChoiceDialog,
    :h-box javafx.scene.layout.HBox,
    :tree-table-column javafx.scene.control.TreeTableColumn,
    :shadow javafx.scene.effect.Shadow,
    :tab javafx.scene.control.Tab,
    :separator javafx.scene.control.Separator,
    :check-menu-item javafx.scene.control.CheckMenuItem,
    :dialog-pane javafx.scene.control.DialogPane,
    :perspective-transform javafx.scene.effect.PerspectiveTransform,
    :arc-to javafx.scene.shape.ArcTo,
    :cell javafx.scene.control.Cell,
    :polyline javafx.scene.shape.Polyline,
    :phong-material javafx.scene.paint.PhongMaterial,
    :media javafx.scene.media.Media,
    :glow javafx.scene.effect.Glow,
    :parallel-transition javafx.animation.ParallelTransition,
    :toggle-group javafx.scene.control.ToggleGroup,
    :scatter-chart javafx.scene.chart.ScatterChart,
    :reflection javafx.scene.effect.Reflection,
    :menu javafx.scene.control.Menu,
    :tooltip javafx.scene.control.Tooltip,
    :rectangle javafx.scene.shape.Rectangle,
    :tree-table-view javafx.scene.control.TreeTableView,
    :list-cell javafx.scene.control.ListCell,
    :line-chart javafx.scene.chart.LineChart,
    :text-formatter javafx.scene.control.TextFormatter,
    :slider javafx.scene.control.Slider,
    :polygon javafx.scene.shape.Polygon,
    :sepia-tone javafx.scene.effect.SepiaTone,
    :scene javafx.scene.Scene,
    :titled-pane javafx.scene.control.TitledPane,
    :text javafx.scene.text.Text,
    :scroll-bar javafx.scene.control.ScrollBar,
    :light-point javafx.scene.effect.Light$Point})

#_
(clojure.pprint/pprint (zipmap (keys cljfx.fx/keyword->lifecycle-delay)
             (repeat 'javafx)))

#_(getter :button :text)


(defmacro getter [cls meth & args]
  (let [instance-sym (with-meta (gensym "instance") {:tag (if (keyword? cls)
                                                            (keyword->class-sym cls)
                                                            cls)})
        getter-expr (if (keyword? meth)
                      (symbol (apply str ".get" (map str/capitalize (-> meth
                                                                        name
                                                                        (str/split #"-")))))
                      meth)]
    `(fn [~instance-sym]
       (~getter-expr ~instance-sym ~@args))))

(getter :button .getText)
(getter :button :text)
(getter :button :tex)
(getter :button .asdf)

{:testfx/assert :button
 :button {:textfx/query :button
          :lookup ".button"}
 :checks [:is-cancel-button
          :is-not-cancel-button
          :is-default-button
          :is-not-default-button
          ]
 }

;{:testfx/assert :combo-box}
;{:testfx/assert :dimension-2d}
;{:testfx/assert :labeled}
;{:testfx/assert :list-view}
;{:testfx/assert :node}
;{:testfx/assert :parent}
;{:testfx/assert :styleable}
;{:testfx/assert :table-view}
;{:testfx/assert :text}
;{:testfx/assert :text-flow}
;{:testfx/assert :text-input-control}
;{:testfx/assert :window}

(comment
(def fx-packages
  '[javafx.animation
    javafx.application
    javafx.beans
    javafx.beans.binding
    javafx.beans.property
    javafx.beans.property.adapter
    javafx.beans.value
    javafx.collections
    javafx.concurrent
    javafx.embed.swing
    javafx.embed.swt
    javafx.event
    javafx.fxml
    javafx.geometry
    javafx.scene
    javafx.scene.canvas
    javafx.scene.chart
    javafx.scene.control
    javafx.scene.control.cell
    javafx.scene.effect
    javafx.scene.image
    javafx.scene.input
    javafx.scene.layout
    javafx.scene.media
    javafx.scene.paint
    javafx.scene.shape
    javafx.scene.text
    javafx.scene.transform
    javafx.scene.web
    javafx.stage
    javafx.util
    javafx.util.converter
    netscape.javascript])

(defn find-fx-classes []
  (into {}
        (map (fn [kw]
               (let [simple-str (case kw
                                  :svg-path "SVGPath"
                                  :html-editor "HTMLEditor"
                                  :light-spot "Light$Spot"
                                  :list-spinner-value-factory "SpinnerValueFactory$ListSpinnerValueFactory"
                                  :xy-chart-series "XYChart$Series"
                                  :xy-chart-data "XYChart$Data"
                                  :double-spinner-value-factory "SpinnerValueFactory$DoubleSpinnerValueFactory"
                                  :integer-spinner-value-factory "SpinnerValueFactory$IntegerSpinnerValueFactory"
                                  :pie-chart-data "PieChart$Data"
                                  :light-distant "Light$Distant"
                                  :light-point "Light$Point"
                                  (apply str (map str/capitalize (-> kw
                                                                     name
                                                                     (str/split #"-")))))
                     full-sym (some (fn [pkg]
                                      (let [cls-str (str pkg "." simple-str)]
                                        (when (try (Class/forName cls-str)
                                                   (catch Exception _))
                                          (symbol cls-str))))
                                    fx-packages)]
                 (assert full-sym kw)
                 [kw full-sym]
                 )))
        (keys cljfx.fx/keyword->lifecycle-delay)
       ))

(require 'clojure.pprint)

(clojure.pprint/pprint (find-fx-classes))
)
