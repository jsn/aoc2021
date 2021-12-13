(ns d13
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- parse-fold [s]
  (let [[p1 p2] (str/split s #"=")
        coord (if (= p1 "fold along x") 0 1)]
    [coord (Integer/parseInt p2)]))

(defn- parse [fname]
  (let [[dots folds] (-> fname slurp (str/split #"\n\s*?\n"))
        dots (->> dots string->vector (partition 2) (map vec) set)
        folds (->> folds str/split-lines (map parse-fold))]
    [dots folds]))

(defn- flip-one [c v]
  (if (<= c v) c (- v (- c v))))

(defn- fold-one [p [coord v]]
  (update p coord flip-one v))

(defn- fold [points fld]
  (set (map #(fold-one % fld) points)))

(defn one [s]
  (let [[points folds] (parse s)]
    (->> folds first (fold points) count)))

(deftest t1
  (is (= 17 (one "d13t.in"))))

(defn print-page [points]
  (doseq [y (range (inc (apply max (map second points))))]
    (println
      (apply str
        (for [x (range (inc (apply max (map first points))))]
          (if (points [x y]) "#" " "))))))

(defn two [s]
  (->> s parse (apply reduce fold) print-page))

(defn -main [& args]
  (let [input (or (first args) (str *ns* ".in"))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
