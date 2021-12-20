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
        lines (vec (str/split-lines img))
        w (count (first lines))
        h (count lines)]
    {:rules rules
     :bg \.
     :cells (into {}
              (for [x (range w) y (range h)
                    :let [c (get-in lines [y x])]]
                [[x y] c]))}))

(defn- next-char [{:keys [bg rules cells]} [px py]]
  (get rules
    (Integer/parseInt
      (apply str
        (for [y (range (dec py) (+ 2 py))
              x (range (dec px) (+ 2 px))]
          (if (= \# (cells [x y] bg)) \1 \0)))
      2)))

(defn- step [{:keys [bg rules cells] :as world}]
  (let [xs (map first (keys cells))
        ys (map second (keys cells))]
    (assoc world
      :bg (if (= bg \.) (first rules) (get rules 511))
      :cells
      (into {}
        (for [x (range (dec (apply min xs)) (+ 2 (apply max xs)))
              y (range (dec (apply min ys)) (+ 2 (apply max ys)))
              :let [c (next-char world [x y])]]
          [[x y] c])))))

(defn- solve [s n]
  (let [worlds (iterate step (parse s))]
    (-> worlds (nth n) :cells vals frequencies (get \#))))

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
