(ns d00
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn one [s]
  "not implemented.")

(defn two [s]
  "not implemented")

(defn -main [& args]
  (let [input (slurp (or (first args) "d00.in"))]
    (println "1." (one input))
    (println "2." (two input))))
