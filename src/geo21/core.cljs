(ns geo21.core
    (:require [figwheel.client :as fw]
              [om.core :as om :include-macros true]
              [om-tools.dom :as dom :include-macros true]
              [geo21.math :refer [ref-point polygon-screen-coordinates pull split-polygon]]
              [clojure.string :as string]))


(enable-console-print!)


(defn by-id
  "Get element by id"
  [id]
  (.getElementById js/document (name id)))

(def shared-state (atom { :dragging nil}))

(def app-state
  (atom
    {:elements
     {
      :600 {:id :600 :type :polygon :data [{:x 100 :y 100} {:x 500 :y 100} {:x 500 :y 500} {:x 100 :y 500}] :translate-x 0 :translate-y 0 :rotate 0 }
     }
     }))

(def app-history (atom [@app-state]))

(defn polygons
  []
  (filter
   #(= :polygon (:type %)) (vals (:elements (om/root-cursor app-state)))))

(defn element-being-dragged-id []
 (:id (:dragging @shared-state)))

(defn element-being-dragged-dx []
 (:dx (:dragging @shared-state)))

(defn element-being-dragged-dy []
 (:dy (:dragging @shared-state)))

(defn element-being-dragged []
  (when (element-being-dragged-id)
    (om/ref-cursor ((element-being-dragged-id) (:elements (om/root-cursor app-state))))))


(defn element [keyw]
   (keyw (:elements (om/root-cursor app-state))))


(defn segment-view [segment owner]
  (let [to (element (:to segment))
        from (element (:from segment))]
    (reify
      om/IDidMount
      (did-mount [_]
        (om/observe owner (om/ref-cursor to))
        (om/observe owner (om/ref-cursor from)))
      om/IRender
      (render [_]
        (dom/g nil
          (dom/line {
             :x1 (:x from)
             :y1 (:y from)
             :x2 (:x to)
             :y2 (:y to)
             :strokeWidth "1"
             :opacity "0.5"
             :stroke "black"
            }))))))

(defn build-segment [segment]
  {:from {:x (:x (element (:from segment)))
          :y (:y (element (:from segment)))}
   :to   {:x (:x (element (:to segment)))
          :y (:y (element (:to segment)))}})

(defn build-point [point]
  {:x (:x point) :y (:y point)})

(defn segments-from-point
  [point]
    (filter
      #(and (= :segment (:type %)) (or (= (:id point) (:to %)) (= (:id point) (:from %))))
        (vals (:elements (om/root-cursor app-state)))))

(defn segments-x-polygons
  [point]
  (for [s (segments-from-point point)
        p (polygons)]
        {:segment s :polygon p}))


(defn split-polygon-update [to-do]
  (map (fn [action]
    (om/transact! (om/root-cursor app-state) :elements
      (fn [elements]
        (let [coming-0  (nth (:coming action) 0)
              coming-1  (nth (:coming action) 1)]
          (assoc (dissoc elements (:id (:going action)))
            (:id coming-0) coming-0
            (:id coming-1) coming-1
            ))))) to-do))

(defn perform [action]
     (vec action))

(defn drop-point
  [point]
  (let [what  (for [s (segments-from-point point) p (polygons)]{:segment s :polygon p})
        to-do (vec (remove nil? (map
                (fn [pair]
                  (when-let [p (split-polygon (:polygon pair) (build-segment (:segment pair)))]
                    {:going (:polygon pair) :coming p})) what)))]
    (perform (split-polygon-update to-do))
    (om/transact! (om/root-cursor app-state) :elements
      (fn [elements]
        (apply dissoc elements
          (map :id (filter #(or (= :point (:type %)) (= :segment (:type %))) (vals elements))))))
    (swap! app-history conj @app-state)))



(defn point-view [point owner]
  (reify
    om/IRender
    (render [_]
        (dom/circle  {:r 6
                        :id (:id point)
                        :cx (:x point)
                        :cy (:y point)}))))


(defn polygon-view [polygon owner]
  (reify
    om/IRender
    (render [_]
        (dom/g  {:transform (str "translate(" (:translate-x polygon) "," (:translate-y polygon) ")rotate(" (:rotate polygon) " " (:x (ref-point polygon)) " " (:y (ref-point polygon)) ")")
                    :id (:id polygon)
                    :className (:className polygon "")
                    :onMouseDown
                    #(do
                       (.stopPropagation %)
                       (let [canvas (by-id "canvas")
                           rect (.getBoundingClientRect canvas)
                           mouseY (- (.-clientY %) (.-top rect))
                           mouseX (- (.-clientX %) (.-left rect))]


                       (swap! shared-state assoc :dragging {:id (:id polygon)
                                                            :dx (- (:x (ref-point polygon)) mouseX)
                                                            :dy (- (:y (ref-point polygon)) mouseY)})
                       ))}
               (dom/polygon {:points (clojure.string/join " " (map #(str (:x %) "," (:y %)) (:data polygon)))})
               (dom/circle  {:r 10
                            :cx (:x (ref-point polygon))
                            :cy (:y (ref-point polygon))})))))





(defmulti element-view (fn [element owner] (:type element)))


(defmethod element-view :segment
  [element owner] (segment-view element owner))

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
  (do
    (om/update! element [:x] (+ x (element-being-dragged-dx)))
    (om/update! element [:y] (+ y (element-being-dragged-dy)))))

(defmethod update-element :polygon
  [element x y]
  (let  [pol (polygon-screen-coordinates element)
         transform (pull element {:x (- 0 (element-being-dragged-dx))
                                  :y (- 0 (element-being-dragged-dy))}  {:x x :y y})]

    (om/update! element [:className] "dragging")
    (om/update! element [:translate-x] (:x (:offset transform)))
    (om/update! element [:translate-y] (:y (:offset transform)))
    (om/update! element [:rotate] (:angle transform))))


(defmulti release-element (fn [element]
    (when-not (nil? element) (:type element))))

(defmethod release-element nil [_])

(defmethod release-element :point
  [element]
  (drop-point element)
  (swap! shared-state assoc :dragging nil))

(defmethod release-element :polygon
  [element]
  (let [polygon (polygon-screen-coordinates element)]
        (swap! shared-state assoc :dragging nil)
        (om/update! element [:className] nil)
        (om/update! element [:data] (:data polygon))
        (om/update! element [:translate-x] 0)
        (om/update! element [:translate-y] 0)
        (om/update! element [:rotate] 0))
        (swap! app-history conj @app-state))


(defn elements-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div {:id "Elements view"}
        (dom/button {:onClick
            #(let []
              (when (> (count @app-history) 1)
                    (swap! app-history pop)
                    (reset! app-state (last @app-history))))} "Undo")
        (dom/svg
               {
                :id "canvas"
                  :onMouseDown
                #(let [canvas (by-id "canvas")
                       rect (.getBoundingClientRect canvas)
                       mouseY (- (.-clientY %) (.-top rect))
                       mouseX (- (.-clientX %) (.-left rect))
                       timestamp  (.getTime (js/Date.))
                       from {:id (keyword (str "f-" timestamp)) :type :point :x mouseX :y mouseY}
                       to {:id (keyword (str "t-" timestamp)) :type :point :x mouseX :y mouseY}
                       segment {:id (keyword (str "s-" timestamp)) :type :segment :from (:id from) :to (:id to)}]

                       (om/transact! (om/root-cursor app-state) :elements
                          (fn [elements]
                            (assoc elements
                              (:id from) from
                              (:id to) to
                              (:id segment) segment
                              )))

                       (swap! shared-state assoc :dragging {:id (:id to)
                             :dx (- (:x (ref-point to)) mouseX)
                             :dy (- (:y (ref-point to)) mouseY)})

                   )

                :onMouseUp
                #(release-element (element-being-dragged))
                :onMouseMove
                #(let [canvas (by-id "canvas")
                       rect (.getBoundingClientRect canvas)
                       mouseY (- (.-clientY %) (.-top rect))
                       mouseX (- (.-clientX %) (.-left rect))]
                   (update-element (element-being-dragged) mouseX mouseY))
                }
                  (dom/defs  {:dangerouslySetInnerHTML  {:__html
                    "<filter id=\"innershadow\" x0=\"-50%\" y0=\"-50%\" width=\"200%\" height=\"200%\">
                      <feGaussianBlur in=\"SourceAlpha\" stdDeviation=\"5\" result=\"blur\"></feGaussianBlur>
                      <feOffset dy=\"2\" dx=\"3\"></feOffset>
                      <feComposite in2=\"SourceAlpha\" operator=\"arithmetic\" k2=\"-1\" k3=\"1\" result=\"shadowDiff\"></feComposite>
                      <feFlood flood-color=\"#666\" flood-opacity=\"0.75\"></feFlood>
                      <feComposite in2=\"shadowDiff\" operator=\"in\"></feComposite>
                      <feComposite in2=\"SourceGraphic\" operator=\"over\" result=\"firstfilter\"></feComposite>
                      <feGaussianBlur in=\"firstfilter\" stdDeviation=\"5\" result=\"blur2\"></feGaussianBlur>
                      <feOffset dy=\"-2\" dx=\"-3\"></feOffset>
                      <feComposite in2=\"firstfilter\" operator=\"arithmetic\" k2=\"-1\" k3=\"1\" result=\"shadowDiff\"></feComposite>
                      <feFlood flood-color=\"#666\" flood-opacity=\"0.75\"></feFlood>
                      <feComposite in2=\"shadowDiff\" operator=\"in\"></feComposite>
                      <feComposite in2=\"firstfilter\" operator=\"over\"></feComposite>
                    </filter>"
                  }} nil)
                  (om/build-all element-view
                      (vals (:elements data))))))))


(om/root elements-view app-state
  {:target (. js/document (getElementById "elements"))})


(fw/start {
           :load-warninged-code true  ;; <- Add this
           :on-jsload (fn []
                        ;; (stop-and-start-my app)
                        )})
