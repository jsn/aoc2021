(ns d20
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- parse [s]
  (let [[rules img] (-> s slurp (str/split #"\n\s*\n"))
        lines (vec (str/split-lines img))]
    {:rules rules
     :bg \.
     :cells (into {}
              (for [x (range (count (first lines)))
                    y (range (count lines))]
                [[x y] (get-in lines [y x])]))}))

(defn- neighbs [[x y]]
  (for [dy [-1 0 1] dx [-1 0 1]] [(+ x dx) (+ y dy)]))

(defn- next-char [{:keys [bg rules cells]} p]
  (get rules
    (Integer/parseInt
      (apply str
        (for [[x y] (neighbs p)]
          ({\# 1} (cells [x y] bg) 0)))
      2)))

(defn- step [{:keys [bg rules cells] :as world}]
  (assoc world
    :bg (if (= bg \.) (first rules) (get rules 511))
    :cells
    (into {}
      (for [[x y] (->> cells keys (mapcat neighbs) set)]
        [[x y] (next-char world [x y])]))))

(defn- solve [s n]
  (let [steps (iterate step (parse s))]
    (-> steps (nth n) :cells vals frequencies (get \#))))

(defn one [s]
  (solve s 2))

(deftest t1
  (is (= 35 (one "d20t.in"))))

(defn two [s]
  (solve s 50))

(deftest t2
  (is (= 3351 (two "d20t.in"))))

(defn -main [& args]
  (let [input (or (first args) (str *ns* ".in"))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
