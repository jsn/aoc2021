(ns d04
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- read-input [fname]
  (let [[nums & tail] (-> fname slurp (str/split #"\n"))
        nums (string->vector nums)
        boards (->> tail (partition 6) (mapv #(mapv string->vector (rest %))))]
    [nums boards]))

(deftest t-in
  (let [[nums boards] (read-input "d04t.in")]
    (is (= (count boards) 3))
    (is (= (boards 1)
          [[ 3 15  0  2 22]
           [ 9 18 13 17  5]
           [19  8  7 25 23]
           [20 11 10 24  4]
           [14 21 16 12  6]]))))

(defn- make-player [board]
  (let [indices (for [y (range 5) x (range 5)] [y x])
        r-ind (zipmap (map #(get-in board %) indices) indices)]
    (assert (= 25 (count r-ind)))
    {:board board
     :r-ind r-ind
     :seen #{}}))

(defn- make-move [{:keys [seen board r-ind] :as player} n]
  (if-let [[y x] (r-ind n)]
    (let [seen (conj seen [y x])
          win? (or
                 (every? seen (for [x (range 5)] [y x]))
                 (every? seen (for [y (range 5)] [y x])))]
      (cond-> (assoc player :seen seen)
        win? (assoc :bingo n)))
    player))

(defn- score [{:keys [seen board r-ind bingo] :as player}]
  (let [unmarked (for [[n pos] r-ind :when (not (seen pos))] n)]
    (* (apply + unmarked) bingo)))

(defn one [s]
  (let [[nums boards] (read-input s)]
    (loop [[n & tail] nums
           players (mapv make-player boards)]
      (have! n)
      (let [players (mapv #(make-move % n) players)
            winners (filter :bingo players)]
        (case (count winners)
          0 (recur tail players)
          1 (score (first winners))
          (throw (ex-info "Too many winners!" {:winners winners})))))))

(deftest t1
  (is (= 4512 (one "d04t.in"))))

(defn two [s]
  (let [[nums boards] (read-input s)]
    (loop [won []
           [n & tail] nums
           players (mapv make-player boards)]
      (if-not n (score (peek won))
        (let [players (mapv #(make-move % n) players)
              winners (filter :bingo players)]
          (if (seq winners)
            (recur (into won winners) tail (remove :bingo players))
            (recur won tail players)))))))

(deftest t2
  (is (= 1924 (two "d04t.in"))))

(defn -main [& args]
  (let [input (or (first args) (str *ns* ".in"))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
