(ns d08
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
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

(defn- has-len-of [digit] #(= (count %) (count (PATTERNS digit))))

(defn- has-segments [segments] #(set/superset? % (set segments)))

(defn- has-no-segments [segments] #(not-any? (set segments) %))

(defn- is-not [digits] #(not ((set digits) %)))

(defn- select-one [preds haystack]
  (let [xs (filter (apply every-pred preds) haystack)]
    (assert (= 1 (count xs)))
    (first xs)))

(def ^:private test1 "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defn- solve2 [line]
  (let [[patterns nums] (parse-line line)
        p1 (select-one [(has-len-of 1)] patterns)
        p4 (select-one [(has-len-of 4)] patterns)
        p7 (select-one [(has-len-of 7)] patterns)
        p8 (select-one [(has-len-of 8)] patterns)
        segment-freqs (->> patterns
                        (mapcat vec)
                        frequencies
                        (map (fn [[k v]] [v k]))
                        (into {}))
        b (segment-freqs 6)
        e (segment-freqs 4)
        f (segment-freqs 9)
        c (first (set/difference p1 #{f}))
        p9 (select-one [(has-len-of 9) (has-no-segments [e])] patterns)
        p5 (select-one [(has-len-of 5) (has-segments [b])] patterns)
        p2 (select-one [(has-len-of 2) (has-segments [e])] patterns)
        p3 (select-one [(has-len-of 3) (is-not [p2 p5])] patterns)
        p6 (select-one [(has-len-of 6) (has-no-segments [c])] patterns)
        p0 (select-one [(has-len-of 0) (is-not [p6 p9])] patterns)
        m {p0 0 p1 1 p2 2 p3 3 p4 4 p5 5 p6 6 p7 7 p8 8 p9 9}
        ]
    (->> nums (map m) (apply str) Integer/parseInt)))

(deftest t2
  (is (= 5353 (solve2 test1))))

(defn two [s]
  (->> s slurp str/split-lines (map solve2) (apply +)))

(deftest t2b
  (is (= 61229 (two "d08t.in"))))

(defn -main [& args]
  (let [input (or (first args) (str *ns* ".in"))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
