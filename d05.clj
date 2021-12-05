(ns d05
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn- straight-line [[x1 y1 _ x2 y2]]
  (cond
    (= x1 x2) (for [y (range (min y1 y2) (inc (max y1 y2)))] [x1 y])
    (= y1 y2) (for [x (range (min x1 x2) (inc (max x1 x2)))] [x y1])
    :else nil))

(defn- solve [line-fn s]
  (->> s
    string->vector
    (partition 5)
    (keep line-fn)
    (apply concat)
    frequencies
    (remove #(= 1 (val %)))
    count))

(defn one [s]
  (solve straight-line s))

(deftest t1
  (is (= 5 (one test1))))

(defn- make-range [v1 v2]
  (cond
    (> v2 v1) (range v1 (inc v2))
    (< v2 v1)  (range v1 (dec v2) -1)
    :else (repeat v1)))

(defn- any-line [[x1 y1 _ x2 y2]]
  (mapv vector (make-range x1 x2) (make-range y1 y2)))

(defn two [s]
  (solve any-line s))

(deftest t2
  (is (= 12 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
