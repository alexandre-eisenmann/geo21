(ns geo21.math)

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

(defn clean-segments [v]
  (filter #(or (not= (-> % :to :x) (-> % :from :x)) (not= (-> % :to :y) (-> % :from :y))) v))

(defn same-points? [p1 p2]
  (and (= (:x p1) (:x p2)) (= (:y p1) (:y p2))))

(defn split-polygon
  "Split a polygon by a segment when appropriated"
  [polygon segment]
  (let [polygon-screen (polygon-screen-coordinates polygon)
        marked (flatten (map (fn [side]
         (let [p (intersection side segment)]
           (if (and (some? p) (not (same-points? p (:to side))))
             [{:to (:to side) :from p} :break {:to p :from (:from side)}]
             side))) (polygon-segments polygon-screen)))
        partioned (partition-by #(= % :break) marked)]
      (when (= (count partioned) 5)
        (let [n (name (:id polygon-screen))
              a (keyword (str n "a"))
              b (keyword (str n "b"))]
        [(polygon-from-segments a (clean-segments (vec (nth partioned 2))))
         (polygon-from-segments b (clean-segments (concat (vec (nth partioned 4)) (vec (nth partioned 0)))))]))))

(defn pull
  "Pull a polygon in direction of x and y"
  [polygon sourcePosition to]
  (let [centroid (ref-point polygon)
        com (ref-point (polygon-screen-coordinates polygon))
        r1 (magnitude sourcePosition)]
     (if (>= r1 10)
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



