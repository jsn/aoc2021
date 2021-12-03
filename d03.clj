(ns d03
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn- most-freq [sq]
  (->> sq
    frequencies
    (sort-by val)
    (map first)))

(defn one [s]
  (let [strs (-> s (str/split #"\s+"))
        len (count (first strs))
        freqs (->> len range (map (fn [i] (most-freq (map #(get % i) strs)))))
        gamma (Integer/parseInt (->> freqs (map first) (apply str)) 2)
        epsilon (Integer/parseInt (->> freqs (map last) (apply str)) 2)]
    (* gamma epsilon)))

(deftest t1
  (is (= 198 (one test1))))

(defn- oxy-fn [freqs]
  (if (>= (freqs \1) (freqs \0)) \1 \0))

(defn- co2-fn [freqs]
  (if (<= (freqs \0) (freqs \1)) \0 \1))

(defn- narrow [strs spec-fn]
  (let [len (count (first strs))]
    (loop [strs strs
           bit 0]
      (have! #(> len %) bit)
      (let [choosen (spec-fn (frequencies (map #(get % bit) strs)))
            strs (have! seq (filter #(= (get % bit) choosen) strs))]
        (if (= 1 (count strs))
          (Integer/parseInt (first strs) 2)
          (recur strs (inc bit)))))))

(defn two [s]
  (let [strs (-> s (str/split #"\s+"))]
    (* (narrow strs oxy-fn) (narrow strs co2-fn))))

(deftest t2
  (is (= 230 (two test1))))

(defn -main [& args]
  (let [input (slurp (str *ns* ".in"))]
    (println "1." (one input))
    (println "2." (two input))))
