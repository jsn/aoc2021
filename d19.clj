(ns d19
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- parse-scanner [block]
  {:bs (->> block str/split-lines rest (mapv string->vector))})

(defn- parse [fname]
  (mapv parse-scanner
    (-> fname slurp (str/split #"\n\s*?\n"))))

(defn- gen1 [v]
  (for [i (range 1 3) j (range 3)
        :when (not= i j)
        :let [k (first (set/difference #{0 1 2} #{i j}))]]
    (-> v
      (assoc i (v j))
      (assoc j (v i))
      (update k -))))

(defn- gen [vs]
  (into vs (mapcat gen1 vs)))

(def ^:private ROTS
  (nth (iterate gen #{[1 2 3]}) 4))

(defn- xlate1 [d v]
  (if (pos? d) (get v (dec d)) (- (xlate1 (- d) v))))

(defn- xlate [rot v]
  (mapv #(xlate1 % v) rot))

(def ^:private v- #(mapv - %1 %2))
(def ^:private v+ #(mapv + %1 %2))

(defn- rotate-bview [rot bview]
  (set (map #(xlate rot %) bview)))

(defn- add-permutations [{:keys [bs] :as scanner}]
  (let [views0 (mapv #(set (for [b bs] (v- b %))) bs)]
    (assoc scanner :bviews
      (into {}
        (map
          (fn [rot]
            [rot (mapv #(rotate-bview rot %) views0)]) ROTS)))))

(defn- bviews-match? [bv1 bv2]
  (>= (count (set/intersection bv1 bv2)) 12))

(defn- sviews-match? [sv1 sv2]
  (first
    (for [i1 (range (count sv1))
          i2 (range (count sv2))
          :when (bviews-match? (sv1 i1) (sv2 i2))]
      [i1 i2])))

(defn- align-scanners [sc1 sc2]
  (let [sview1 (get-in sc1 [:bviews (:dir sc1)])]
    (first
      (for [[dir2 sview2] (:bviews sc2)
            :let [[i1 i2] (sviews-match? sview1 sview2)]
            :when i1]
        [dir2 i1 i2]))))

(defn- beacon-xlated [sc dir i]
  (xlate dir (get-in sc [:bs i])))

(defn- solve* [scs]
  (loop [scs scs
         seen #{}]
    (let [[sc1 sc2]
          (first
            (for [sc1 (range (count scs))
                  :when (get-in scs [sc1 :dir])
                  sc2 (range (count scs))
                  :when (and
                          (not (get-in scs [sc2 :dir]))
                          (not (seen [sc1 sc2])))]
              [sc1 sc2]))]
      (if-not sc1 scs
        (let [seen (conj seen [sc1 sc2])
              [dir2 i1 i2] (align-scanners (scs sc1) (scs sc2))]
          ; (prn ::trying sc1 sc2)
          (if dir2
            (let [dir1 (get-in scs [sc1 :dir])
                  b1 (beacon-xlated (scs sc1) dir1 i1)
                  b2 (beacon-xlated (scs sc2) dir2 i2)
                  off2 (v- (v+ b1 (get-in scs [sc1 :off])) b2)

                  scs (update scs sc2 merge {:dir dir2 :off off2})]
              ; (prn ::match)
              (recur scs seen))
            (recur scs seen))
          )))))

(def ^:private solve (memoize solve*))

(defn- normalized-beacons [{:keys [dir off bs]}]
  (map #(v+ off (xlate dir %)) bs))

(defn one [s]
  (let [scs (->> s parse (map add-permutations) vec)
        scs (update scs 0 merge {:dir [1 2 3] :off [0 0 0]})]
    (->> scs
      solve
      (mapcat normalized-beacons)
      set
      count)))

(deftest t1
  (is (= 79 (one "d19t.in"))))

(defn- distance [sc1 sc2]
  (let [o1 (:off sc1)
        o2 (:off sc2)]
    (apply + (map #(Math/abs ^long %) (v- o1 o2)))))

(defn two [s]
  (let [scs (->> s parse (map add-permutations) vec)
        scs (solve (update scs 0 merge {:dir [1 2 3] :off [0 0 0]}))]
    (apply max
      (for [sc1 scs sc2 scs]
        (distance sc1 sc2)))))

(deftest t2
  (is (= 3621 (two "d19t.in"))))

(defn -main [& args]
  (let [input (or (first args) (str *ns* ".in"))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
