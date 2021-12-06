(ns d06
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "3,4,3,1,2")

(defn- input->states [s]
  (reduce
    #(update %1 %2 inc)
    (vec (repeat 9 0))
    (string->vector s)))

(defn- step [[z & tail]]
  (-> tail vec (conj z) (update 6 + z)))

(defn- solve [s n]
  (->> s
    input->states
    (iterate step)
    (drop n)
    first
    (apply +)))

(deftest t1
  (is (= 26 (solve test1 18)))
  (is (= 5934 (solve test1 80))))

(defn one [s]
  (solve s 80))

(deftest t2
  (is (= 26984457539 (solve test1 256))))

(defn two [s]
  (solve s 256))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
