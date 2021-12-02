(ns d02
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1 "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(def DIRS
  {"forward" [1 0]
   "down" [0 1]
   "up" [0 -1]})

(defn one [s]
  (apply *
    (reduce
      (fn [pos [dir off]]
        (let [dir (DIRS dir :undefined)
              off (Integer/parseInt off)]
          (mapv + pos (map #(* off %) dir))))
      [0 0]
      (partition 2 (str/split s #"\s+")))))

(deftest t1
  (is (= 150 (one test1))))

(defn two [s]
  (let [[x y]
        (reduce
          (fn [[x y a] [dir off]]
            (let [off (Integer/parseInt off)]
              (case (keyword dir)
                :down [x y (+ a off)]
                :up [x y (- a off)]
                :forward [(+ x off) (+ y (* a off)) a])))
          [0 0 0]
          (partition 2 (str/split s #"\s+")))]
    (* x y)))

(deftest t2
  (is (= 900 (two test1))))

(defn -main [& args]
  (let [input (slurp "d02.in")]
    (println "1." (one input))
    (println "2." (two input))))
