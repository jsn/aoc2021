(ns d11
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
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defn- parse-input [s]
  (->> s str/split-lines (mapv (fn [l] (mapv #(- (int %) (int \0)) l)))))

(defn- neighbs [[y x]]
  (for [dx [-1 0 1] dy [-1 0 1]
        :when (not (= dx dy 0))
        :let [x' (+ x dx) y' (+ y dy)]
        :when (and (<= 0 x' 9) (<= 0 y' 9))]
    [y' x']))

(def ^:private ALL (for [x (range 10) y (range 10)] [y x]))

(defn- inc-points [board points]
  (loop [board board
         [p & points] points
         flashed #{}]
    (if-not p [board flashed]
      (let [board (update-in board p inc)
            v (get-in board p)
            flashed (cond-> flashed (> v 9) (conj p))]
        (recur board points flashed)))))

(defn- turn [board]
  (loop [board board
         seen #{}
         queue ALL]
    (let [[board flashed] (inc-points board queue)]
      (if-not (seq flashed)
        (let [board (reduce #(assoc-in %1 %2 0) board seen)]
          [board seen])
        (let [flashed (set/difference flashed seen)
              seen (set/union seen flashed)]
          (recur board seen (mapcat neighbs flashed)))))))

(defn one [s]
  (let [board (parse-input s)]
    (loop [step 0
           board board
           cnt 0]
      (let [[board flashed] (turn board)
            cnt (+ cnt (count flashed))
            step (inc step)]
        (if (= step 100) cnt
          (recur step board cnt))))))

(deftest t1
  (is (= 1656 (one test1))))

(defn two [s]
  (let [board (parse-input s)]
    (loop [step 0
           board board
           cnt 0]
      (let [[board flashed] (turn board)
            step (inc step)]
        (if (= 100 (count flashed)) step
          (recur step board cnt))))))

(deftest t2
  (is (= 195 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
