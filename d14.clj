(ns d14
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- parse [fname]
  (let [[tmpl tail] (-> fname slurp (str/split #"\n\s*?\n"))
        rules (->> tail str/split-lines (map #(str/split % #"\s+->\s+")))]
    [tmpl (into {} rules)]))

(defn- step [rules tmpl]
  (->> tmpl
    seq
    (partition 2 1)
    (mapcat (fn [[a b]] (if-let [c (rules (str a b))] [c b] [b])))
    (apply str (first tmpl))))

(defn- score [freqs]
  (- (apply max freqs) (apply min freqs)))

(defn one [s]
  (let [[tmpl rules] (parse s)]
    (->> tmpl
      (iterate #(step rules %))
      (drop 10)
      first
      seq
      frequencies
      vals
      score)))

(deftest t1
  (is (= 1588 (one "d14t.in"))))

(def ^:private add0 (fnil + 0))

(defn- step2 [rules pairs]
  (->> pairs
    (mapcat
      (fn [[[a b :as pair] cnt]]
        (if-let [c (rules pair)]
          [[(str a c) cnt] [(str c b) cnt]]
          [[pair cnt]])))
    (reduce
      (fn [h [k v]] (update h k add0 v))
      {})))

(defn- pairs->freqs [tmpl pairs]
  (reduce
    (fn [h [[a b] cnt]] (update h b add0 cnt))
    {(first tmpl) 1}
    pairs))

(defn- solve [s steps]
  (let [[tmpl rules] (parse s)]
    (->> tmpl
      seq
      (partition 2 1)
      (map #(apply str %))
      frequencies
      (iterate #(step2 rules %))
      (drop steps)
      first
      (pairs->freqs tmpl)
      vals
      score)))

(deftest t2
  (is (= (one "d14t.in") (solve "d14t.in" 10))))

(defn two [s]
  (solve s 40))

(deftest t3
  (is (= 2188189693529 (two "d14t.in"))))

(defn -main [& args]
  (let [input (or (first args) (str *ns* ".in"))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
