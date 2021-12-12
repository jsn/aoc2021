(ns d12
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def ^:private test2
"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(def ^:private conj* (fnil conj #{}))

(defn- input->world [s]
  (reduce
    (fn [world [n1 n2]]
      (-> world
        (update n1 conj* n2)
        (update n2 conj* n1)))
    {}
    (partition 2 (str/split s #"[-\s]"))))

(defn- is-large? [cave]
  (< (int (first cave)) (int \a)))

(defn- can-visit1 [path]
  (let [seen (->> path (remove is-large?) set)]
    #(not (seen %))))

(defn- gen-paths [world guard-fn]
  (loop [[path & paths] [["start"]]
         ready []]
    (if-not path ready
      (if (= "end" (peek path))
        (recur paths (conj ready path))
        (let [can-visit? (guard-fn path)
              more (->> path
                      peek
                      world
                      (filter can-visit?)
                      (mapv #(conj path %)))]
          (recur (apply conj more paths) ready))))))

(defn one [s]
  (-> s input->world (gen-paths can-visit1) count))

(deftest t1
  (is (= 10 (one test1)))
  (is (= 226 (one test2))))

(defn- can-visit2 [path]
  (let [seen (->> path (remove is-large?) frequencies)
        seen2 (->> seen vals (apply max) (= 2))]
    #(and (not= "start" %) (or (not seen2) (not (seen %))))))

(defn two [s]
  (-> s input->world (gen-paths can-visit2) count))

(deftest t2
  (is (= 36 (two test1)))
  (is (= 3509 (two test2))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
