;; Advent of Code challenge, 2020 day 7
(ns advent.bags
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp]))

;; parse a single line of the input
(defn parse-bag-definition
  [line]
  (let [bag-def (first (re-seq #"([\S\s]+) bags contain([\S\s]+)" line))
        name (nth bag-def 1)
        contents-def (re-seq #"([\d]+)\s([\w\s]+)\sbag[s]?[.,]" (nth bag-def 2))
        contents (map #(vec [(nth % 2) (Integer/parseInt (nth % 1))]) contents-def)]
    {name (into {} contents)}))

;; find the set of bags that contain at least 1 of any of the input set
(defn find-containing-bags
  [target-bags bag-defs]
  (let [target-contains-key (fn [key] (contains? target-bags key))]
    (filter #(some target-contains-key (keys (bag-defs %))) (keys bag-defs))))

(defn count-bag-contents
  [bag-name bag-count bag-defs]
  (reduce (fn [c [k v]] (+ c (* bag-count (count-bag-contents k v bag-defs))))
    bag-count (bag-defs bag-name)))

(defn -main []
  (with-open [input (clojure.java.io/reader "day7-bags.txt")]
    (let [bag-defs (into {} (map #(parse-bag-definition %) (line-seq input)))]
      ;; print bag definitions for inspection
      ;;(pp/pprint bag-defs)

      ;; find all bags that contain "shiny gold" directly or indirectly
      (let [all-bags (loop [bags #{"shiny gold"}]
                       (let [expanded-bags (set (concat (find-containing-bags bags bag-defs) bags))]
                         (if (= expanded-bags bags) bags (recur expanded-bags))))]
      ;;(pp/pprint all-bags)

      ;; output for part 1
      (println (format "%d bags contain \"shiny gold\" bag (including itself)" (count all-bags)))

      ;; output for part 2
      (println (format "1 \"shiny gold\" bag transitively contains %d bags" (count-bag-contents "shiny gold" 1 bag-defs)))))))