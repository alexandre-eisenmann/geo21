(ns geo21.core
  (:require [figwheel.client :as fw]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]))

(enable-console-print!)

(extend-type string
  ICloneable
  (-clone [s] (js/String. s)))

(extend-type js/String
  ICloneable
  (-clone [s] (js/String. s))
  om/IValue
  (-value [s] (str s)))

(def shared-state
  (atom
    {
     :dragging nil
    }))


(def app-state
  (atom
    {
     :elements
     {
      :100 {:type :point :x 100 :y 100}
      :200 {:type :point :x 15 :y 28}
;      :300 {:type :point :x 35 :y 18}
;      :400 {:type :point :x 55 :y 8}
;      :500 {:type :polygon :data [{:x 110 :y 20} {:x 115 :y 50} {:x 150 :y 7}]}
;      :600 {:type :polygon :data [{:x 119 :y 84} {:x 170 :y 102} {:x 120 :y 170} {:x 18 :y 118}]}
     }
     }))




(defn element-being-dragged-id []
 (:id (:dragging @shared-state)))

(defn element-being-dragged-dx []
 (:dx (:dragging @shared-state)))

(defn element-being-dragged-dy []
 (:dy (:dragging @shared-state)))

(defn element-being-dragged []
  (when (element-being-dragged-id)
    (om/ref-cursor ((element-being-dragged-id) (:elements (om/root-cursor app-state))))))

(defn point-view [point owner]
  (reify
    om/IRender
    (render [_]
        (dom/circle #js {:r 40
                        :stroke "black"
                        :strokeWidth 1
                        :fill "red"
                        :id (name (:id point))
                        :onMouseDown
                         #(do
                            (swap! shared-state assoc :dragging {:id (:id point)
                                              :dx (- (:x point) (- (.-clientX %) 8))
                                              :dy (- (:y point) (- (.-clientY %) 64))})
                            ;(.log js/console (str (:x point) " " (.-clientX %) " " (.-clientY %)))
                            )
                        :cx (:x point)
                        :cy (:y point)}))))

(defn polygon-view [polygon owner]
  (reify
    om/IRender
    (render [_]
       (dom/polygon #js {:points (clojure.string/join " " (map #(str (:x %) "," (:y %)) (:data polygon)))}))))


(defmulti element-view (fn [element owner] (:type element)))

(defmethod element-view :point
  [element owner] (point-view element owner))

(defmethod element-view :polygon
  [element owner] (polygon-view element owner))


(defmulti entry-view (fn [person _] (:type person)))


(defn elements-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "Elements view"}
        (dom/h2 nil "Elements")
        (apply dom/svg #js
               {
                :onMouseLeave
                #(swap! shared-state assoc :dragging nil)
                :onMouseUp
                #(swap! shared-state assoc :dragging nil)
                :onMouseMove
                #(do
                   (when-let [elem (element-being-dragged)]
                   (let [top 64;(.. % -target -offsetTop)
                         left 8;(.. % -target -offsetLeft)
                         ]
                     (om/update! elem [:x] (+ (- (.-clientX %) left) (element-being-dragged-dx)))
                     (om/update! elem [:y] (+ (- (.-clientY %) top)  (element-being-dragged-dy))))))
                }
          (let [m (:elements data)]
          (om/build-all element-view (map #(assoc (% m) :id %) (keys m)))))))))


(om/root elements-view app-state
  {:target (. js/document (getElementById "elements"))})


(fw/start {
           :load-warninged-code true  ;; <- Add this
           :on-jsload (fn []
                        ;; (stop-and-start-my app)
                        )})
