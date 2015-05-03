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
      :100 {:id :100 :type :point :x 100 :y 100}
      :200 {:id :200 :type :point :x 15 :y 28}
      :300 {:id :300 :type :point :x 35 :y 18}
      :400 {:id :400 :type :point :x 55 :y 8}
      :450 {:id :450 :type :point :x 10 :y 10}
      :460 {:id :460 :type :point :x 20 :y 20}
      :500 {:id :500 :type :polygon :data [{:x 110 :y 20} {:x 115 :y 50} {:x 150 :y 7}] :translate-x 0 :translate-y 0 :rotate 0}
      :600 {:id :600 :type :polygon :data [{:x 119 :y 84} {:x 170 :y 102} {:x 120 :y 170} {:x 18 :y 118}] :translate-x 0 :translate-y 0 :rotate 0}
      :700 {:id :700 :type :segment :from :450 :to :460}
     }
     }))

(defn cross-product
  "Cross product between two points"
  [p1 p2]
  (- (* (:x p1) (:y p2)) (* (:y p1) (:x p2))))

(defn add
  "Vectorial adding between two points"
  [p1 p2]
  {:x (+ (:x p1) (:x p2)) :y (+ (:y p1) (:y p2))})


(defn times
  "Multiply a point by a scalar"
  [p k]
  {:x (* (:x p) k) :y (* (:y p) k)})

(def s1 {:from {:x -2 :y 0} :to {:x 2 :y 0}})
(def s2 {:from {:x 0 :y -2} :to {:x 0 :y 2}})
(def s3 {:from {:x 5 :y -2} :to {:x 5 :y 3}})
(def s4 {:from {:x -10 :y -10} :to {:x 12 :y 11}})
(def s5 {:from {:x 119 :y 84} :to {:x 170 :y 102}})
(def s6 {:from {:x 170 :y 102} :to {:x 120 :y 170}})
(def s7 {:from {:x 120 :y 170} :to {:x 18 :y 118}})
(def s8 {:from {:x 18 :y 118} :to {:x 119 :y 84}})
(def s9 {:from {:x 0 :y 0} :to {:x 500 :y 500}})
(def s10 {:from {:x 0 :y 100} :to {:x 100 :y 100}})

(defn intersection
  "Interesection between two segments. Return nil when there is no intersection"
  [s1 s2]
  (let [s1p1 (:from s1)
        s1p2 (:to s1)
        s2p1 (:from s2)
        s2p2 (:to s2)
        s1p1x (:x s1p1)
        s1p1y (:y s1p1)
        s1p2x (:x s1p2)
        s1p2y (:y s1p2)
        s2p1x (:x s2p1)
        s2p1y (:y s2p1)
        s2p2x (:x s2p2)
        s2p2y (:y s2p2)
        p s1p1
        r {:x (- s1p2x s1p1x) :y (- s1p2y s1p1y)}
        q s2p1
        s {:x (- s2p2x s2p1x) :y (- s2p2y s2p1y)}
        qMinusP {:x (- (:x q) (:x p)) :y (- (:y q) (:y p))}
        rXs (cross-product r s)
        u (/ (cross-product qMinusP r) rXs)
        t (/ (cross-product qMinusP s) rXs)]
          (if (and (not (zero? rXs)) (>= t 0) (<= t 1) (>= u 0) (<= u 1))
            (add p (times r t)))))

(defn polygon-segments
  "Return segments of polygon"
  [polygon ]
  (let [ps (:data polygon)]
    (map (fn [p1 p2] {:to p1 :from p2}) ps (conj (vec (rest ps)) (first ps)))))

(defn polygon-from-segments
  "Build polygon from a vector of segments"
  [id segments]
  (let [f (first segments)]
    {:id id :type :polygon :data (vec (concat  [(:to f)] [(:from f)] (map :from (rest segments)))) :translate-x 0 :translate-y 0 :rotate 0}))

(defn split-polygon
  "Split a polygon by a segment when appropriated"
  [polygon segment]
  (let [marked (flatten (map (fn [side]
         (if-let [p (intersection side segment)]
           [{:to (:to side) :from p} :break {:to p :from (:from side)}]
           side)) (polygon-segments polygon)))
        partioned (partition-by #(= % :break) marked)]
      (when (= (count partioned) 5)
        (let [n (name (:id polygon))]
        [(polygon-from-segments (keyword (str n "-a")) (vec (nth partioned 2)))
         (polygon-from-segments (keyword (str n "-b")) (concat (vec (nth partioned 4)) (vec (nth partioned 0))))]))))


(defn segments-from-point
  [point]
    (filter
      #(and (= :segment (:type %)) (or (= (:id point) (:to %)) (= (:id point) (:from %))))
        (vals (:elements (om/root-cursor app-state)))))



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

(defmulti ref-point (fn [element] (:type element)))

(defmethod ref-point :segment
  [segment] nil)

(defmethod ref-point :point
  [point] (select-keys point [:x :y]))

(defmethod ref-point :polygon
  [polygon]
  (let [m (:data polygon)
        c (count m)]
       {:x (/ (reduce + (map :x m)) c)
        :y (/ (reduce + (map :y m)) c)}))

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
          (dom/line #js {
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

(defn point-view [point owner]
  (reify
    om/IRender
    (render [_]
        (dom/circle #js {:r 5
                        :stroke "black"
                        :strokeWidth 1
                        :fill "red"
                        :id (:id point)
                        :onMouseLeave
                        #()
                        :onMouseUp
                        (fn [_] (let [what
                                (for [s (segments-from-point point) p (polygons)]
                                  {:segment s :polygon p})
                                to-do
                                  (vec (remove nil? (map
                                   (fn [pair]
                                     (when-let [p (split-polygon (:polygon pair) (build-segment (:segment pair)))]
                                       {:going (:polygon pair) :coming p}))
                                   what)))]
;                                  (.log js/console (str to-do))
                                  (perform (split-polygon-update to-do))


                                 ))
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
                         :transform (str "translate(" (:translate-x polygon) "," (:translate-y polygon) ")rotate(" (:rotate polygon) " " (:x (ref-point polygon)) " " (:y (ref-point polygon)) ")")
                        :id (:id polygon)
                        :onMouseDown
                           #(do

                              (swap! shared-state assoc :dragging {:id (:id polygon)
                                              :dx (- (+ (:x (ref-point polygon)) (:translate-x polygon)) (- (.-clientX %) 8))
                                              :dy (- (+ (:y (ref-point polygon)) (:translate-y polygon)) (- (.-clientY %) 64))})
;                              (.log js/console (str @shared-state))
                            )
                           }
                    ))))


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
                #(do
                   ;(.log js/console (str " bbbbbbb" (build-point (element :460))))
                   (swap! shared-state assoc :dragging nil))
                :onMouseMove
                #(do
;                   (.log js/console (str "xxxxx"  (element-being-dragged)))
                   (update-element (element-being-dragged) (.-clientX %) (.-clientY %)))
                }
            (om/build-all element-view (vals (:elements data)))
;            (om/build-all element-view (map #(assoc (% m) :id %) (keys m))))
               )))))


(om/root elements-view app-state
  {:target (. js/document (getElementById "elements"))})


(fw/start {
           :load-warninged-code true  ;; <- Add this
           :on-jsload (fn []
                        ;; (stop-and-start-my app)
                        )})
