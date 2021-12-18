(ns d17
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "target area: x=20..30, y=-10..-5")

(defn- parse [s]
  (let [[x1 x2 y1 y2]
        (->> s
          str/trim
          (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
          rest
          (map #(Integer/parseInt %)))]
    (assert (< y1 y2 0))
    [x1 x2 y1 y2]))

(defn- vy [[_ _ y1 _]]
  (reduce + 0 (range (dec (- y1)) 0 -1)))

(defn one [s]
  (-> s parse vy))

(deftest t1
  (is (= 45 (one test1))))

(defn- vx->hits [[x1 x2 _ _] vx]
  (let [xs (reductions + 0 (range vx 0 -1))
        times (->> xs (map #(when (<= x1 %2 x2) %1) (range)) (remove nil?))]
    (when (seq times)
      (let [t-min (first times)
            t-last (last times)
            t-max (when (not= vx t-last) t-last)]
        [t-min t-max]))))

(defn- vxs [[x1 x2 _ _ :as target]]
  (keep #(when-let [hits (vx->hits target %)] [% hits]) (range x2 0 -1)))

(defn- vy->hits [[_ _ y1 y2 :as target] ^long vy]
  (if (pos? vy)
    (mapv #(+ 1 (* 2 vy) %) (vy->hits target (- -1 vy)))
    (loop [rv []
           t 0
           y 0
           vy vy]
      (let [y (+ y vy)
            t (inc t)
            vy (dec vy)]
        (cond
          (< y y1) rv
          (<= y y2) (recur (conj rv t) t y vy)
          :else (recur rv t y vy))))))

(defn- vys [[_ _ y1 y2 :as target]]
  (keep #(when-let [hits (vy->hits target %)] [% hits]) (range y1 (- y1))))

(defn two [s]
  (let [target (parse s)]
    (count
      (for [[vx [t-min t-max]] (vxs target)
            [vy ts] (vys target)
            :when (some #(<= t-min % (or t-max %)) ts)]
        [vx vy]))))

(deftest t2
  (is (= 112 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
