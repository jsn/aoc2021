(ns d16
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1 "D2FE28")
(def ^:private test2 "38006F45291200")
(def ^:private test3 "EE00D40C823060")

(defn- hex->bins [x]
  (-> x str (Integer/parseInt 16) (+ 16) (Integer/toString 2) rest))

(defn- parse [input]
  (->> input
    str/trim
    seq
    (mapcat hex->bins)))

(deftest t1
  (is (= "110100101111111000101000" (apply str (parse test1)))))

(defn- take-bits [stream n]
  (let [[head stream] (split-at n stream)
        head (apply str head)]
    (assert (= n (count head)))
    [(Integer/parseInt head 2) stream]))

(defn- take-literal [stream]
  (loop [rv 0
         cnt 0
         stream stream]
    (let [[more & stream] stream
          [int4 stream] (take-bits stream 4)
          rv (bit-or (bit-shift-left rv 4) int4)]
      (if (= more \1)
        (recur rv (inc cnt) stream)
        [rv stream]))))

(declare take-packet)

(defn- take-length-ops [stream]
  (let [[len stream] (take-bits stream 15)
        [mine stream] (split-at len stream)]
    (assert (= len (count mine)))
    (loop [rv []
           mine mine]
      (let [[v mine] (take-packet mine)
            rv (conj rv v)]
        (if-not (seq mine)
          [rv stream]
          (recur rv mine))))))

(defn- take-n-ops [stream]
  (let [[n stream] (take-bits stream 11)]
    (loop [rv []
           stream stream]
      (let [[v stream] (take-packet stream)
            rv (conj rv v)]
        (if (= n (count rv))
          [rv stream]
          (recur rv stream))))))

(defn- take-op [stream]
  (let [[kind & stream] stream]
    (if (= \0 kind)
      (take-length-ops stream)
      (take-n-ops stream))))

(defn- take-packet [stream]
  (let [[ver stream] (take-bits stream 3)
        [typ stream] (take-bits stream 3)
        [v stream] (if (= typ 4) (take-literal stream) (take-op stream))]
    [[ver typ v] stream]))

(deftest t1-2
  (is (= [6 4 2021] (-> test1 parse take-packet first))))

(deftest t2-1
  (is (= [1 6 [[6 4 10] [2 4 20]]] (-> test2 parse take-packet first))))

(deftest t3-1
  (is (= [7 3 [[2 4 1] [4 4 2] [1 4 3]]] (-> test3 parse take-packet first))))

(defn- score1 [[v t xs]]
  (if (= t 4) v (->> xs (map score1) (apply + v))))

(defn one [s]
  (-> s parse take-packet first score1))

(deftest t-one
  (is (= 23 (one "C0015000016115A2E0802F182340"))))

(defn- bool->int [v]
  (if v 1 0))

(def ^:private OPS
  {0 +
   1 *
   2 min
   3 max
   5 (comp bool->int >)
   6 (comp bool->int <)
   7 (comp bool->int =)})

(defn- evaluate [[v t xs]]
  (if (= t 4) xs (apply (have! (OPS t)) (map evaluate xs))))

(defn two [s]
  (-> s parse take-packet first evaluate))

(deftest t-two
  (is (= (two "CE00C43D881120") 9))
  (is (= (two "9C0141080250320F1802104A08") 1)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
