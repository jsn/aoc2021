(ns d25
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")

(defn- parse [s]
  (let [lines (vec (str/split-lines s))
        h (count lines)
        w (count (first lines))
        cs (into {}
               (for [x (range w) y (range h)
                     :let [c (get-in lines [y x])]
                     :when (not= c \.)]
                 [[x y] c]))]
    {:dims [w h] :cs cs}))

(defn- move1 [{:keys [dims cs]} c p]
  (when (= c (cs p))
    (let [i (have! ({\> 0 \v 1} c))
          p' (update p i #(mod (inc %) (dims i)))]
      (when-not (cs p')
        [p' c p]))))

(defn- move-all [{:keys [cs] :as world} c]
  (let [moved (->> cs keys (keep #(move1 world c %)))
        cs' (apply dissoc cs (map peek moved))]
    (assoc world :cs (into cs' (map pop moved)))))

(defn- world->str [{:keys [dims cs]}]
  (str/join "\n"
    (for [y (range (dims 1))]
      (str/join
        (for [x (range (dims 0))]
          (cs [x y] \.))))))

(defn one [s]
  (loop [i 0
         w (parse s)]
    (let [w' (-> w (move-all \>) (move-all \v))
          i (inc i)]
      (if (= w' w) i
        (recur i w')))))

(deftest t1
  (is (= 58 (one test1))))

(defn two [s]
  "hey, it's Christmas!")

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
