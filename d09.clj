(ns d09
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn- parse-line [l]
  (mapv #(- (int %) (int \0)) l))

(defn- parse-input [s]
  (let [lines (str/split-lines s)
        h (count lines)
        w (count (first lines))
        rows (mapv #(parse-line %) lines)]
    {:w w :h h :rows rows}))

(defn- neighbs [{:keys [w h]} [y x]]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
    (map #(mapv + % [y x]))
    (remove (fn [[y' x']] (or (neg? x') (neg? y') (= x' w) (= y' h))))))

(defn- low-point? [{:keys [w h rows] :as world} p]
  (every? #(< (get-in rows p) (get-in rows %)) (neighbs world p)))

(defn one [s]
  (let [{:keys [w h rows] :as world} (parse-input s)

        lows
        (for [x (range w) y (range h)
              :when (low-point? world [y x])]
          (get-in rows [y x]))]
    (apply + (map inc lows))))

(deftest t1
  (is (= 15 (one test1))))

(defn- flow [{:keys [rows] :as world} [y x]]
  (->> [y x]
    (neighbs world)
    (sort-by #(get-in rows %))
    first))

(defn- grow-basin [world flows [border inner]]
  (let [border'
        (->> border
          (mapcat #(neighbs world %))
          set
          (filter #(and (not (inner %)) (inner (flows %))))
          set)]
    [border' (set/union inner border')]))

(defn two [s]
  (let [{:keys [w h rows] :as world} (parse-input s)
        lows (set
               (for [x (range w) y (range h)
                     :when (low-point? world [y x])] [y x]))

        flows (into {} 
                (for [x (range w) y (range h)
                      :let [p [y x]]
                      :when (not= 9 (get-in rows p))]
                  [p (if (lows p) p (flow world p))]))]
    (loop [basins (map #(vector #{%} #{%}) lows)]
      (if-not (some seq (map first basins))
        (->> basins
          (map (comp count peek))
          sort
          reverse
          (take 3)
          (apply *))
        (recur (map #(grow-basin world flows %) basins))))))

(deftest t2
  (is (= 1134 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
