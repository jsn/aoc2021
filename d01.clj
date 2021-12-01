(ns d01
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "199
200
208
210
200
207
240
269
260
263")

(defn one [s]
  (->> s
    string->vector
    (partition 2 1)
    (map #(apply - %))
    (filter neg?)
    count))

(deftest t1
  (is (= (one test1) 7)))

(defn two [s]
  (->> s
    string->vector
    (partition 3 1)
    (map #(apply + %))
    (partition 2 1)
    (map #(apply - %))
    (filter neg?)
    count))

(deftest t2
  (is (= (two test1) 5)))

(defn -main [& args]
  (let [input (slurp "d01.in")]
    (println "1." (one input))
    (println "2." (two input))))
