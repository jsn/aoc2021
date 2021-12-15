(ns d15
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.data.priority-map :as prio]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defn- char->int [c]
  (- (int c) (int \0)))

(defn- parse [s]
  (let [lines (str/split-lines s)
        h (count lines)
        w (count (first lines))
        rows (mapv #(mapv char->int %) lines)]
    {:w w :h h :rows rows}))

(defn- neighbs [{:keys [w h]} [y x]]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
    (map #(mapv + % [y x]))
    (remove (fn [[y' x']] (or (neg? x') (neg? y') (= x' w) (= y' h))))))

(defn- dist [{:keys [rows]} _ p]
  (get-in rows p))

(defn- h-fn [{:keys [w h]} [y x]]
  (+ (- w x 1) (- h y 1)))

(defn- generate-route [node came-from]
  (loop [route '()
         node node]
    (if (came-from node)
      (recur (cons node route) (came-from node))
      route)))

(defn- route
  [graph dist h start goal]
  (loop [visited {}
         queue (prio/priority-map-keyfn first start [0 0 nil])]
    (when (seq queue)
      (let [[current [_ current-score previous]] (peek queue)
            visited (assoc visited current previous)]
        (if (= current goal)
          (generate-route goal visited)
          (recur
            visited
            (reduce (fn [queue node]
                      (let [score (+ current-score (dist current node))]
                        (if (and
                              (not (contains? visited node))
                              (or
                                (not (contains? queue node))
                                (< score (get-in queue [node 1]))))
                          (assoc queue node [(+ score (h node)) score current])
                          queue)))
              (pop queue)
              (graph current))))))))

(defn- solve [{:keys [w h rows] :as world}]
  (let [graph #(neighbs world %)
        dist #(dist world %1 %2)
        h-fn #(h-fn world %)
        path (route graph dist h-fn [0 0] [(dec h) (dec w)])]
    (apply + (map #(get-in rows %) path))))

(defn one [s]
  (solve (parse s)))

(deftest t1
  (is (= 40 (one test1))))

(defn- add-risk [& args]
  (inc (mod (apply + -1 args) 9)))

(defn- tile [{:keys [w h rows]}]
  {:w (* 5 w)
   :h (* 5 h)
   :rows
   (into []
     (for [ty (range 5) y (range w)]
       (into []
         (for [tx (range 5) x (range w)]
           (add-risk (get-in rows [y x]) tx ty)))))})

(defn two [s]
  (solve (tile (parse s))))

(deftest t2
  (is (= 315 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
