(ns d22
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")

(defn- parse-line [l]
  (let [[_ state & coords]
        (re-matches
          #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)"
          l)
        coords (mapv #(Integer/parseInt %) coords)]
    [(= (have! state) "on") coords]))

(defn- parse [s]
  (->> s str/split-lines (map parse-line)))

(defn- execute1 [space [on [x1 x2 y1 y2 z1 z2]]]
  (let [ps (for [x (range x1 (inc x2))
                 y (range y1 (inc y2))
                 z (range z1 (inc z2))]
             [x y z])]
    ((if on set/union set/difference) space (set ps))))

(defn- in50? [[_ coords]]
  (every? #(<= -50 % 50) coords))

(defn one [s]
  (->> s parse (filter in50?) (reduce execute1 #{}) count))

(deftest t1
  (is (= 39 (one test1))))

(defn- intersect1 [ax1 ax2 bx1 bx2]
  (let [cx1 (max ax1 bx1)
        cx2 (min ax2 bx2)]
    (when (<= cx1 cx2)
      (into [[cx1 cx2]]
        (for [[x1 x2]
              [[ax1 (dec bx1)]
               [(inc bx2) ax2]]
              :when (<= x1 x2)]
          [x1 x2])))))

(defn- intersect [[ax1 ax2 ay1 ay2 az1 az2] [bx1 bx2 by1 by2 bz1 bz2]]
  (for [xb (intersect1 ax1 ax2 bx1 bx2)
        yb (intersect1 ay1 ay2 by1 by2)
        zb (intersect1 az1 az2 bz1 bz2)]
    (into xb (concat yb zb))))

(defn- collide [coords1 coords2]
  (let [[mid & tail] (intersect coords1 coords2)]
    (if (not mid) [coords1] tail)))

(defn- execute2 [space [on coords]]
  (cond-> (set (mapcat #(collide % coords) space))
    on (conj coords)))

(defn- volume [[x1 x2 y1 y2 z1 z2]]
  (* (- x2 x1 -1) (- y2 y1 -1) (- z2 z1 -1)))

(defn two [s]
  (->> s parse (reduce execute2 #{}) (map volume) (apply +)))

(deftest t2
  (is (= 2758514936282235 (two (slurp "d22t.in")))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
