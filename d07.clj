(ns d07
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "16,1,2,0,4,2,7,1,2,14")

(defn- median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (/ (+ bottom-val top-val) 2)))))

(defn one [s]
  (let [xs (string->vector s)
        pos (median xs)]
    (->> xs (map #(Math/abs ^long (- pos %))) (apply +))))

(defn- cost2 [pos x]
  (let [n (Math/abs ^long (- pos x))] (/ (* n (inc n)) 2)))

(defn two [s]
  (let [xs (string->vector s)
        pos (/ (apply + xs) (count xs))
        poses (set [(Math/floor pos) (Math/ceil pos)])]
    (->> poses
      (map #(apply + (map (partial cost2 %) xs)))
      (apply min))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
