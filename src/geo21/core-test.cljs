(ns geo21.core-test
    (:require [geo21.math :refer [matrix-multiplication split-polygon intersection area centroid]]))


(def p1 {:id :333 :type :point :x 300 :y 10})
(def p2 {:id :444 :type :point :x 300 :y 600})
(def p3 {:id :777 :type :point :x 100 :y 100})
(def p4 {:id :888 :type :point :x 500 :y 100})
(def p5 {:id :999 :type :point :x 0 :y 0})
(def p6 {:id :111 :type :point :x 700 :y 700})


(def s1 {:from p1 :to p2})
(def s2 {:from p3 :to p4})
(def s3 {:from p5 :to p6})

(def polygon {:id :666 :type :polygon
              :data [{:x 100 :y 100} {:x 500 :y 100} {:x 500 :y 500} {:x 100 :y 500}]
              :translate-x 0 :translate-y 0 :rotate 0 })

(defn all-cycles [v]
  (let [size (count v)
        vv (take (* 2 size) (cycle v))]
          (for [x (range size)] (subvec (vec vv) x (+ x size)))))

(defn same-cycle? [a b]
  (let [size (count a)
        aa (take (* 2 size) (cycle a))]
        (some #(= % b)
          (for [x (range size)] (subvec (vec aa) x (+ x size))))))

(defn matrix-test []
  (let [A [[ 1 2 ][3 4]]
        I [[ 1 0 ][0 1]]
        B [[ 3 1 ][2 9]]]
  (assert (= (matrix-multiplication A I) A)
            "Matrix times identity should return the matrix itself."))

  (let [M (matrix-multiplication A B)]
    (assert (= M [[7 19] [17 39]])
              (str "Matrix multiplication failed " A  " x " B " != " M))))

(defn assert-equality
  [a b e1 e2]
  (assert (= (set (concat (all-cycles a) (all-cycles b)))
             (set (concat (all-cycles e1) (all-cycles e2))))
          "Split polygon fail"))


(defn split-test []
  (let [[pol-a pol-b] (map :data (split-polygon polygon s1))
        expected-2 [{:x 300, :y 500} {:x 300, :y 100} {:x 500, :y 100} {:x 500, :y 500}]
        expected-1 [{:x 300, :y 500} {:x 100, :y 500} {:x 100, :y 100} {:x 300, :y 100}]]
    (.log js/console (str pol-a " " pol-b))
        (assert-equality pol-a pol-b expected-1 expected-2)))

(defn split-test-2 []
  (let [[pol-a pol-b] (map :data (split-polygon polygon s3))
        expected-2 [{:x 500, :y 500} {:x 100, :y 500} {:x 100, :y 100}]
        expected-1 [{:x 100, :y 100} {:x 500, :y 100} {:x 500, :y 500}]]
    (.log js/console (str pol-a " " pol-b))
        (assert-equality pol-a pol-b expected-1 expected-2)))

(defn area-test []
  (do
    (assert (= (area polygon) 160000))
    (assert (= (area {:data [{:x 100 :y 100} {:x 500 :y 100} {:x 500 :y 500}]}) 80000))))

(defn centroid-test []
    (assert (= (centroid polygon) {:x 300 :y 300})))




