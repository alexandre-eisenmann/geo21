  (ns geo21.core
  (:require [figwheel.client :as fw]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]))




(enable-console-print!)

(def shared-state (atom { :dragging nil }))

(def app-state
  (atom
    {
     :elements
     {
      :200 {:id :200 :type :point :x 15 :y 28}
      :300 {:id :300 :type :point :x 35 :y 18}
      :400 {:id :400 :type :point :x 55 :y 8}
      :450 {:id :450 :type :point :x 10 :y 10}
      :460 {:id :460 :type :point :x 20 :y 20}
      :600 {:id :600 :type :polygon :data [{:x 100 :y 100} {:x 500 :y 100} {:x 500 :y 500} {:x 100 :y 500}] :translate-x 0 :translate-y 0 :rotate 0}
      :700 {:id :700 :type :segment :from :450 :to :460}
     }
     }))

(defn matrix-multiplication
  "Matrix multiplication A x B"
  [A B]
  (let [rows-number  (count B)
        cols-number  (first (map count B))
        rows-verif   (apply = (count B) (map count A))
        cols-verif   (apply = (map count B))]
    (if (and rows-verif cols-verif)
      (let [rows (range rows-number)
            cols (range cols-number)
            r (map (fn [col] (map (fn [row]
                  (reduce + (map * (nth A row)
                      (map (fn [line] (nth line col)) B)))) rows)) cols)]
         (vec (map (fn [row] (vec (map #(nth % row) r))) rows))))))

(defn translate-matrix
  "Return a matrix capable of translate a point"
  [x y]
  [ [1 0 x] [0 1 y] [0 0 1] ])

(defn translate-point
  "Translate points to x and y"
  [point x y]
  (let [result (flatten (matrix-multiplication (translate-matrix x y) [[(:x point)] [(:y point)] [1]]))]
    {:x (nth result 0) :y (nth result 1)}))

(defn rotate-matrix
  "Return a matrix capable of rotate through [0 0] or an arbitrary point"
  ([teta]
    (let [cosTeta  (.cos js/Math (* teta (/ (.-PI js/Math) 180)))
          sinTeta  (.sin js/Math (* teta (/ (.-PI js/Math) 180)))]
      [ [cosTeta (- 0 sinTeta) 0] [sinTeta cosTeta 0] [0 0 1] ]))
  ([teta x y]
   (matrix-multiplication (translate-matrix x y)
     (matrix-multiplication (rotate-matrix teta) (translate-matrix (- 0 x) (- 0 y))))))

(defn polygon-screen-coordinates
  "Screen coordinates for a polygon"
  [polygon]
  (let [centroid (ref-point polygon)
        M (matrix-multiplication (translate-matrix (:translate-x polygon) (:translate-y polygon))
                                 (rotate-matrix (:rotate polygon) (:x centroid) (:y centroid)))]

    (assoc polygon :translate-x 0 :translate-y 0 :rotate 0 :data
      (vec (map (fn [p] (let [coord (flatten (matrix-multiplication M [[(:x p)] [(:y p)] [1]]))]
                          {:x (nth coord 0) :y (nth coord 1)})) (:data polygon))))))



(defn distance
  "Distance between two points"
  [p1 p2]
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))]
    (.sqrt js/Math (+ (* dx dx) (* dy dy)))))

(defn magnitude
  "Magnitude of a vector"
  [v]
  (distance v {:x 0 :y 0}))


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


(defn pull
  "Pull a polygon in direction of x and y"
  [polygon sourcePosition to]
  (let [centroid (ref-point polygon)
        ;com (translate-point (ref-point polygon) (:translate-x polygon) (:translate-y polygon))
        com (ref-point (polygon-screen-coordinates polygon))
        r1 (magnitude sourcePosition)]
     (if (> r1 10)
       (let [r2 (magnitude {:x (- (:x to) (:x com)) :y (- (:y to) (:y com))})
             c1 {:x (+ (:x to) (/ (* r1 (- (:x to) (:x com))) r2)),
                 :y (+ (:y to) (/ (* r1 (- (:y to) (:y com))) r2))}
             c2 {:x (- (:x to) (/ (* r1 (- (:x to) (:x com))) r2)),
                 :y (- (:y to) (/ (* r1 (- (:y to) (:y com))) r2))}
             d1 (magnitude (add c1 {:x (- 0 (:x com)) :y (- 0 (:y com))}))
             d2 (magnitude (add c2 {:x (- 0 (:x com)) :y (- 0 (:y com))}))
             new-centroid (if (< d2 d1) c2 c1)
             targetPosition {:x (- (:x to) (:x new-centroid)) :y (- (:y to) (:y new-centroid))}
             sourceArgument (.atan2 js/Math (:y sourcePosition) (:x sourcePosition))
             targetArgument (.atan2 js/Math (:y targetPosition) (:x targetPosition))
             sourceAngle (rem (+ 360 (/ (* 180 sourceArgument) (.-PI js/Math))) 360)
             targetAngle (rem (+ 360 (/ (* 180 targetArgument) (.-PI js/Math))) 360)
             angle (rem (+ 360 (- targetAngle sourceAngle)) 360)
             offset {:x (- (:x new-centroid) (:x centroid)) :y (- (:y new-centroid) (:y centroid))}]
         {:offset offset :angle (rem (+ angle 0) 360) })
         {:offset {:x (- (- (:x to) (:x sourcePosition)) (:x centroid))
                   :y (- (- (:y to) (:y sourcePosition)) (:y centroid))}
                   :angle (:rotate polygon)})

       ))


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
  (let [polygon-screen (polygon-screen-coordinates polygon)
        marked (flatten (map (fn [side]
         (if-let [p (intersection side segment)]
           [{:to (:to side) :from p} :break {:to p :from (:from side)}]
           side)) (polygon-segments polygon-screen)))
        partioned (partition-by #(= % :break) marked)]
      (when (= (count partioned) 5)
        (let [n (name (:id polygon-screen))
              a (keyword (str n "a"))
              b (keyword (str n "b"))]
        [(polygon-from-segments a (vec (nth partioned 2)))
         (polygon-from-segments b (concat (vec (nth partioned 4)) (vec (nth partioned 0))))]))))


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

(defn drop-point
  [point]
  (let [what  (for [s (segments-from-point point) p (polygons)]{:segment s :polygon p})
        to-do (vec (remove nil? (map
                (fn [pair]
                  (when-let [p (split-polygon (:polygon pair) (build-segment (:segment pair)))]
                    {:going (:polygon pair) :coming p})) what)))]
    (perform (split-polygon-update to-do))))


(defn point-view [point owner]
  (reify
    om/IRender
    (render [_]
        (dom/circle #js {:r 6
                        :id (:id point)
                        ;:onMouseLeave
                        ;#(drop-point point)
                        :onMouseUp
                        #(drop-point point)
                        :onMouseDown
                         #(let [rect (.. % -target -parentNode getBoundingClientRect)
                                mouseY (- (.-clientY %) (.-top rect))
                                mouseX (- (.-clientX %) (.-left rect))]
        (.log js/console (str mouseX " " mouseY))

                            (swap! shared-state assoc :dragging {:id (:id point)
                                              :dx (- (:x (ref-point point)) mouseX)
                                              :dy (- (:y (ref-point point)) mouseY)})

                            )
                        :cx (:x point)
                        :cy (:y point)}))))



(defn polygon-view [polygon owner]
  (reify
    om/IRender
    (render [_]
      (dom/polygon #js {:className "ants"
                        :points (clojure.string/join " " (map #(str (:x %) "," (:y %)) (:data polygon)))
                        :transform (str "translate(" (:translate-x polygon) "," (:translate-y polygon) ")rotate(" (:rotate polygon) " " (:x (ref-point polygon)) " " (:y (ref-point polygon)) ")")
                        :id (:id polygon)
                        :onMouseDown
                           #(let [pol (polygon-screen-coordinates polygon)
                                  rect (.. % -target -parentNode getBoundingClientRect)
                                  mouseY (- (.-clientY %) (.-top rect))
                                  mouseX (- (.-clientX %) (.-left rect))]

                              (swap! shared-state assoc :dragging {:id (:id pol)
                                              :dx (- (:x (ref-point pol)) mouseX)
                                              :dy (- (:y (ref-point pol)) mouseY)})

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
  (do
    (om/update! element [:x] (+ x (element-being-dragged-dx)))
    (om/update! element [:y] (+ y (element-being-dragged-dy)))))

(defmethod update-element :polygon
  [element x y]
  (let  [pol (polygon-screen-coordinates element)
         transform (pull element {:x (- 0 (element-being-dragged-dx))
                                  :y (- 0 (element-being-dragged-dy))}  {:x x :y y})]

    (om/update! element [:translate-x] (:x (:offset transform)))
    (om/update! element [:translate-y] (:y (:offset transform)))
    (om/update! element [:rotate] (:angle transform))))




(defmulti release-element (fn [element]
    (when-not (nil? element) (:type element))))

(defmethod release-element nil [_])

(defmethod release-element :point
  [element]
  (swap! shared-state assoc :dragging nil))

(defmethod release-element :polygon
  [element]
  (let [polygon (polygon-screen-coordinates element)]
        (swap! shared-state assoc :dragging nil)
        (om/update! element [:data] (:data polygon))
        (om/update! element [:translate-x] 0)
        (om/update! element [:translate-y] 0)
        (om/update! element [:rotate] 0)))


(defn elements-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:id "Elements view"}
        (dom/h2 nil "")
        (apply dom/svg #js
               {
                :onMouseLeave
                #(release-element (element-being-dragged))
                :onMouseUp
                #(release-element (element-being-dragged))
                :onMouseMove
                #(let [rect (.. % -target -parentNode getBoundingClientRect)
                       mouseY (- (.-clientY %) (.-top rect))
                       mouseX (- (.-clientX %) (.-left rect))]
                   (update-element (element-being-dragged) mouseX mouseY))
                }
            (om/build-all element-view (vals (:elements data))))))))


(om/root elements-view app-state
  {:target (. js/document (getElementById "elements"))})


(fw/start {
           :load-warninged-code true  ;; <- Add this
           :on-jsload (fn []
                        ;; (stop-and-start-my app)
                        )})
