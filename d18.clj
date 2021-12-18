(ns d18
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

(defn- parse [s]
  (->> s str/split-lines (map read-string)))

(def ^:private add vector)

(defn- paths [v]
  (mapcat
    (fn [i]
      (let [v' (v i)
            p [i]]
        (if (int? v')
          [p]
          (map #(apply conj p %) (paths v')))))
    (range (count v))))

(defn- redooce [v]
  (loop [v v]
    (if-let [[i0 i1 i2 i3]
             (->> v
               paths
               (#(concat [nil] % [nil]))
               (partition 4 1)
               (filter #(= (count (second %)) 5))
               first)]
      (do
        (assert (= (count i1) (count i2) 5))
        (recur
          (cond-> v 
            i0 (update-in i0 + (get-in v i1))
            i3 (update-in i3 + (get-in v i2))
            true (assoc-in (pop i1) 0))))
      (if-let [p
               (->> v
                 paths
                 (filter #(>= (get-in v %) 10))
                 first)]
        (recur (update-in v p (fn [x] [(quot x 2) (quot (inc x) 2)])))
        v))))

(defn- magnitude [v]
  (if (int? v) v
    (+ (* 3 (magnitude (first v))) (* 2 (magnitude (second v))))))

(defn one [s]
  (->> s parse (reduce (comp redooce add)) magnitude))

(deftest t1
  (is (= 4140 (one test1))))

(defn two [s]
  (let [nums (parse s)]
    (apply max
      (for [n1 nums n2 nums :when (not= n1 n2)]
        (magnitude (redooce (add n1 n2)))))))

(deftest t2
  (is (= 3993 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
