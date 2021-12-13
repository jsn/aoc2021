(ns d08b
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
    [clojure.core.logic :as l]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private PATTERNS
  (into {} (map (juxt key (comp set val))
             {0 "abcefg"
              1 "cf"
              2 "acdeg"
              3 "acdfg"
              4 "bcdf"
              5 "abdfg"
              6 "abdefg"
              7 "acf"
              8 "abcdefg"
              9 "abcdfg"})))

(defn- parse-line [l]
  (let [[patterns _ nums]
        (->> l string->vector (map name) (partition-by #{"|"}))]
    [(mapv set patterns) (mapv set nums)]))

(defn one [s]
  (let [nums (->> s slurp str/split-lines (map parse-line) (mapcat last))
        lens (set (map #(count (PATTERNS %)) [1 4 7 8]))]
    (->> nums (filter #(lens (count %))) count)))

(deftest t1
  (is (= 26 (one "d08t.in"))))

(def ^:private test1 "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defn- solve2 [line]
  (let [[pats nums] (parse-line line)
        targets (->> PATTERNS vals (map vec))
        vars (zipmap "abcdefg" (repeatedly 7 l/lvar))
        vpats (mapv #(mapv vars %) pats)

        [cmap]
        (l/run 1 [q]
          (l/== q vars)
          (l/permuteo (vals vars) (vec "abcdefg"))
          (l/and*
            (map (fn [vpat]
                   (l/or*
                     (keep #(when (= (count vpat) (count %))
                              (l/permuteo % vpat))
                       targets)))
              vpats)))]
    (->> nums
      (mapv #(set (map cmap %)))
      (map (set/map-invert PATTERNS))
      (apply str)
      Integer/parseInt)))

(deftest t2
  (is (= 5353 (solve2 test1))))

(defn two [s]
  (->> s slurp str/split-lines (map solve2) (apply +)))

; (deftest t2b
;   (is (= 61229 (two "d08t.in"))))

(defn -main [& args]
  (let [input (or (first args) "d08.in")]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
