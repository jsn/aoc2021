(ns d24
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- sym->kw [x]
  (cond-> x (symbol? x) keyword))

(defn- parse [s]
  (->> s str/split-lines (map #(mapv sym->kw (string->vector %)))))

(def ^:private REGS (zipmap [:w :x :y :z] (range)))

(defn- execute [{:keys [r i] :as state} [cmd dst src]]
  (let [d (have! (REGS dst))]
    (if (= cmd :inp)
      (if-not (seq i) (reduced (assoc state :error :short))
        (let [i0 (first i)]
          (-> state
            (update :r assoc d i0)
            (update :i subvec 1))))
      (let [s (if (int? src) src (r (have! (REGS src))))]
        (case cmd
          :add (update state :r update d + s)
          :mul (update state :r update d * s)
          :div (if (zero? s) (reduced (assoc state :error :div))
                 (update state :r update d quot s))
          :mod (if (zero? s) (reduced (assoc state :error :div))
                 (update state :r update d mod s))
          :eql (update state :r assoc d (if (= (r d) s) 1 0))
          ))
      )))

(def ^:private test1
"inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2")

(deftest t1
  (is (= [0 1 1 1]
        (:r (reduce execute {:r [0 0 0 0] :i [7]} (parse test1))))))

(defn- run1r [p z d r]
  (get-in (reduce execute {:r [0 0 0 z] :i [d]} p) [:r r]))

(defn- force1 [prog]
  (let [vs
        (for [n (range 9 0 -1)
              z (range 26)
              :let [y (run1r prog z n 2)]
              :when (zero? y)]
          [z n])]
    (when (seq vs) (into {} vs))))

(defn- force-i [prog fixeds d-range z ws]
  (let [i (count ws)]
    (if (= i (count prog)) ws
      (let [p (prog i)
            f (fixeds i)]
        (if f
          (if-let [d (f (mod z 26))]
            (force-i prog fixeds d-range
              (run1r p z d 3) (conj ws d))
            nil)
          (first
            (for [d d-range
                  :let [v (force-i prog fixeds d-range
                            (run1r p z d 3) (conj ws d))]
                  :when v]
              v)))))))

(defn solve [s d-range]
  (let [prog (->> s parse (partition 18) (mapv vec))
        fixeds (mapv force1 prog)]
    (apply str (force-i prog fixeds d-range 0 []))))

(defn one [s]
  (solve s (range 9 0 -1)))

(defn two [s]
  (solve s (range 1 10)))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
