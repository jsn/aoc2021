(ns d21
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"Player 1 starting position: 4
Player 2 starting position: 8")

(defn- parse [s]
  (->> s str/split-lines (map #(-> % last str Integer/parseInt)) vec))

(defn one [s]
  (loop [ps (parse s)
         dice (cycle (range 1 101))
         i 0
         scores [0 0]]
    (let [p (mod i 2)
          [roll dice] (split-at 3 dice)
          ps (update ps p #(inc (mod (+ % (apply + roll) -1) 10)))
          scores (update scores p + (ps p))]
      (if (>= (scores p) 1000)
        (* 3 (inc i) (scores ([1 0] p)))
        (recur ps dice (inc i) scores)))))

(deftest t1
  (is (= 739785 (one test1))))

(def ^:private DIRACS
  (frequencies
    (for [d0 [1 2 3]
          d1 [1 2 3]
          d2 [1 2 3]]
      (+ d0 d1 d2))))

(defn- turn [[ps scores] p roll]
  (let [ps (update ps p #(inc (mod (+ % roll -1) 10)))
        scores (update scores p + (ps p))]
    [ps scores]))

(defn- quantum-step [p [state cnt]]
  (for [[roll freq] DIRACS]
    [(turn state p roll) (* freq cnt)]))

(defn- fold-cnts [states [state cnt]]
  (update states state (fnil + 0) cnt))

(defn- extract-wins [p states [state cnt]]
  (let [[_ scores] state]
    (if (>= (scores p) 21)
      (update states :wins (fnil + 0) cnt)
      (assoc states state cnt))))

(defn two [s]
  (loop [states {[(parse s) [0 0]] 1}
         p 0
         wins [0 0]]
    (let [states (reduce fold-cnts {} (mapcat #(quantum-step p %) states))
          states (reduce #(extract-wins p %1 %2) {} states)
          wins (update wins p + (states :wins 0))
          states (dissoc states :wins)
          p ([1 0] p)]
      (if (seq states)
        (recur states (long p) wins)
        (apply max wins)))))

(deftest t2
  (is (= 444356092776315 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
