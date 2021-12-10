(ns d10
  (:require
    [clojure.test :refer :all]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.pprint :refer [pprint]]
    [taoensso.truss :refer [have!]]
    [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private test1
"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def ^:private PARENS
  {\( \)
   \< \>
   \{ \}
   \[ \]})

(def ^:private SCORES1
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def ^:private RPARENS (set/map-invert PARENS))

(defn- fold-parens [[head & tail :as stack] c]
  (if-let [p1 (RPARENS c)]
    (if (= head p1) tail
      (reduced [:mismatch c stack]))
    (cons c stack)))

(defn one [s]
  (->> s
    str/split-lines
    (map #(reduce fold-parens '() %))
    (keep #(when (= :mismatch (first %)) (SCORES1 (second %))))
    (apply +)))

(deftest t1
  (is (= 26397 (one test1))))

(def ^:private SCORES2
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn- score-completion [stack]
  (reduce #(+ (* 5 %1) (SCORES2 (PARENS %2))) 0 stack))

(defn two [s]
  (let [scores
        (->> s
          str/split-lines
          (map #(reduce fold-parens '() %))
          (remove #(= :mismatch (first %)))
          (map score-completion)
          sort)]
    (->> scores
      (drop (quot (count scores) 2))
      first)))

(deftest t2
  (is (= 288957 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) (str *ns* ".in")))]
    (println "1." (one input))
    (println "2." (two input))))

(comment
  (-main))
