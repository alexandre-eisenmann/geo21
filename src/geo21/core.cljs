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
      :100 {:type :point :x 100 :y 100 :translate-x 0 :translate-y 0}
      :200 {:type :point :x 15 :y 28 :translate-x 0 :translate-y 0}
      :300 {:type :point :x 35 :y 18 :translate-x 0 :translate-y 0}
      :400 {:type :point :x 55 :y 8 :translate-x 0 :translate-y 0}
      :500 {:type :polygon :data [{:x 110 :y 20} {:x 115 :y 50} {:x 150 :y 7}] :translate-x 0 :translate-y 0}
      :600 {:type :polygon :data [{:x 119 :y 84} {:x 170 :y 102} {:x 120 :y 170} {:x 18 :y 118}] :translate-x 0 :translate-y 0}
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

(defmulti ref-point (fn [element] (:type element)))

(defmethod ref-point :point
  [point] (select-keys point [:x :y]))

(defmethod ref-point :polygon
  [polygon]
  (let [m (:data polygon)
        c (count m)]
       {:x (/ (reduce + (map :x m)) c)
        :y (/ (reduce + (map :y m)) c)}))



(defn point-view [point owner]
  (reify
    om/IRender
    (render [_]
        (dom/circle #js {:r 40
                        :stroke "black"
                        :strokeWidth 1
                        :transform (str "translate(" (:translate-x point) "," (:translate-y point) ")")
                        :fill "red"
                        :id (name (:id point))
                        :onMouseDown
                         #(do
                            (swap! shared-state assoc :dragging {:id (:id point)
                                              :dx (- (:x (ref-point point)) (- (.-clientX %) 8))
                                              :dy (- (:y (ref-point point)) (- (.-clientY %) 64))})
                            ;(.log js/console (str (:x point) " " (.-clientX %) " " (.-clientY %)))
                            )
                        :cx (:x point)
                        :cy (:y point)}))))



(defn polygon-view [polygon owner]
  (reify
    om/IRender
    (render [_]
      (dom/polygon #js {:points (clojure.string/join " " (map #(str (:x %) "," (:y %)) (:data polygon)))
                         :transform (str "translate(" (:translate-x polygon) "," (:translate-y polygon) ")")
                         :onMouseDown
                           #(do
                              (swap! shared-state assoc :dragging {:id (:id polygon)
                                              :dx (- (+ (:x (ref-point polygon)) (:translate-x polygon)) (- (.-clientX %) 8))
                                              :dy (- (+ (:y (ref-point polygon)) (:translate-y polygon)) (- (.-clientY %) 64))})
                            )
                           }
                    ))))


(defmulti element-view (fn [element owner] (:type element)))

(defmethod element-view :point
  [element owner] (point-view element owner))

(defmethod element-view :polygon
  [element owner] (polygon-view element owner))


(defmulti entry-view (fn [person _] (:type person)))


(defmulti update-element (fn [element x y]
    (when-not (nil? element) (:type element))))

(defmethod update-element nil [_])
(defmethod update-element :point
  [element x y]
  (let [top 64 ;(.. % -target -offsetTop)
        left 8 ;(.. % -target -offsetLeft)
        ]
    (om/update! element [:x] (+ (- x left) (element-being-dragged-dx)))
    (om/update! element [:y] (+ (- y top)  (element-being-dragged-dy)))))
(defmethod update-element :polygon
  [element x y]
  (let [top 64 ;(.. % -target -offsetTop)
        left 8 ;(.. % -target -offsetLeft)
        dx (+ (- (- x left) (:x (ref-point element))) (element-being-dragged-dx))
        dy (+ (- (- y top) (:y (ref-point element))) (element-being-dragged-dy))
        ]
    (om/update! element [:translate-x] dx)
    (om/update! element [:translate-y] dy)))



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
                #(update-element (element-being-dragged) (.-clientX %) (.-clientY %))
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
