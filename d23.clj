(ns d23
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
    [clojure.data.priority-map :as prio]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(def ^:private ^:dynamic *depth* 2)

(defn- parse [s]
  (->> s
    (re-seq #"[A-D]")
    (map keyword)
    (partition 4)
    reverse
    (apply map vector)
    (zipmap [:A :B :C :D])))

; ###########1#
; #01234567890#
; ###A#B#C#D###
;   #A#B#C#D#
;   #########")

(def ^:private ROOMS [:A :B :C :D])

(def ^:private TOPO {:A 2 :B 4 :C 6 :D 8})

(def ^:private STOPS (->> 11 range (remove (set (vals TOPO))) set))

(def ^:private COSTS {:A 1 :B 10 :C 100 :D 1000})

(defn- path [room hall]
  (have! keyword? room)
  (have! int? hall)
  (let [from (TOPO room)
        d (if (> hall from) 1 -1)]
    (range from hall d)))

(defn- collector? [world room]
  (let [r (world room)]
    (and (< (count r) *depth*) (every? #(= room %) r))))

(defn- emitter? [world room]
  (not-every? #(= room %) (world room)))

(defn- move-out [world room hall]
  (when (and (emitter? world room) (not (world hall)))
    (let [p (path room hall)]
      (when (not-any? world p)
        (let [r (world room)
              a (peek r)]
          [(* (COSTS a) (+ (count p) 1 *depth* (- (count r))))
           (-> world
             (update room pop)
             (assoc hall a))])))))

(defn- move-in [world hall]
  (let [room (world hall)]
    (when (collector? world room)
      (let [p (path room hall)]
        (when (not-any? world p)
          (let [r (world room)]
            [(* (COSTS room) (+ (count p) *depth* (- (count r))))
             (-> world
               (update room conj room)
               (dissoc hall))]))))))

(defn- h-hall [hall a]
  (* (COSTS a) (inc (count (path a hall)))))

(defn- h-room [room a]
  (if (= room a) 0 (* (COSTS a) (inc (count (path a (TOPO room)))))))

(defn- h [world]
  (apply +
    (for [[k v] world]
      (if (int? k)
        (h-hall k v)
        (if (emitter? world k)
          (apply + (map #(h-room k %) v))
          0)))))

(defn- move-in-all [world]
  (loop [cost 0
         world world]
    (let [[c w]
          (first
            (for [[hall a] world
                  :when (int? hall)
                  :let [[c w] (move-in world hall)]
                  :when c]
              [c w]))]
      (if-not c
        (when (pos? cost) [cost world])
        (recur (+ cost ^long c) w)))))

(defn- branches [world]
  (if-let [b (move-in-all world)] [b]
    (for [room ROOMS
          :when (emitter? world room)
          hall STOPS
          :let [b (move-out world room hall)]
          :when b]
      b)))

(defn- route
  [graph h start goal]
  (loop [visited {}
         queue (prio/priority-map-keyfn first start [0 0 nil])]
    (when (seq queue)
      (let [[current [_ current-score previous]] (peek queue)
            visited (assoc visited current previous)]
        (if (= current goal)
          current-score
          (recur
            visited
            (reduce (fn [queue [dist node]]
                      (let [score (+ current-score dist)]
                        (if (and
                              (not (contains? visited node))
                              (or
                                (not (contains? queue node))
                                (< score (get-in queue [node 1]))))
                          (assoc queue node [(+ score (h node)) score current])
                          queue)))
              (pop queue)
              (graph current))))))))

(defn one [s]
  (let [world (parse s)]
    (binding [*depth* (count (:A world))]
      (let [goal (into {} (for [r ROOMS] [r (vec (repeat *depth* r))]))]
        (route branches h world goal)))))

(deftest t1
  (is (= 12521 (one test1))))

(defn- unfold-input [s]
  (let [[head tail] (split-at 3 (str/split-lines s))]
    (str/join "\n" (concat head ["  #D#C#B#A#" "  #D#B#A#C#"] tail))))

(defn two [s]
  (one (unfold-input s)))

(deftest t2
  (is (= 44169 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
