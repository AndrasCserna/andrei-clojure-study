(ns advent.hextile
  (:require [clojure.pprint :as pp]
            [clojure.set :as set :refer [intersection]]))
(defn count-steps
  [line]
  ;; count the number of steps in the 3 east-bound cardinal directions
  (let [steps (re-seq #"[sn]?[we]" line)]
    (reduce (fn [aggr step] (into aggr (case step
                                         "e" {:E (-> :E aggr inc)}
                                         "w" {:E (dec (aggr :E))}
                                         "ne" {:NE (inc (aggr :NE))}
                                         "sw" {:NE (dec (aggr :NE))}
                                         "se" {:SE (inc (aggr :SE))}
                                         "nw" {:SE (dec (aggr :SE))})))
            {:E 0, :NE 0, :SE 0} steps)))
(defn simplify-steps
  [steps]
  ;; make coordinates unique by removing redundant degree of freedom
  ;; SE = E - NE, return vec of [#steps E, #steps NE]
  [(+ (get steps :E) (get steps :SE))
   (- (get steps :NE) (get steps :SE))])
(defn count-map
  [m]
  ;; count all the occurrences of unique elements into a map
  (reduce (fn [counts elem] (conj counts
                                  {elem (-> (get counts elem 0) inc)}))
          {} m))
(def neighbor-offsets [[1 0] [-1 0] [0 1] [0 -1] [1 -1] [-1 1]])
(defn neighbors
  [tile]
  ;; get set of neighboring tiles
  (-> (map #(mapv + tile %) neighbor-offsets) set))
(defn bounds
  [tile-list]
  ;; calculate coordinate bounds of listed tiles
  (let [x (map #(first %) tile-list)
        y (map #(second %) tile-list)]
    {
     :min-x (- (apply min x) 1)
     :max-x (+ (apply max x) 2)
     :min-y (- (apply min y) 1)
     :max-y (+ (apply max y) 2)}))
(defn evolve-state
  [tile-set]
  ;; calculate list of all tiles with coordinates +/- 1 (inclusive) of existing black tiles
  (let [area-bounds (bounds tile-set)
        check-tiles (set (for [x (range (:min-x area-bounds 0) (:max-x area-bounds)) y (range (:min-y area-bounds) (:max-y area-bounds))] [x y]))]
    (set (filter (fn [tile]
                   ;; calculate next state of tile
                   (let [is-black (contains? tile-set tile)
                         num-blacks (-> (set/intersection tile-set (neighbors tile)) count)]
                     (or
                       (= 2 num-blacks)
                       (and is-black (= num-blacks 1))))
                   check-tiles)))))
(defn -main []
  (let [tiles (with-open [input (clojure.java.io/reader "day24-tiles.txt")] (vec (line-seq input)))
        visited-tiles (map #(simplify-steps (count-steps %)) tiles)
        blacks-start (set (keys (filter (fn [[_ v]] (odd? v)) (frequencies visited-tiles))))]
    ;;(pp/pprint visited-tiles)
    ;;(pp/pprint (count-map visited-tiles))
    ;;(pp/pprint blacks-start)
    (println (format "Black starting tiles: %d" (count blacks-start)))
    (let [blacks-final (reduce (fn [state _] (evolve-state state)) blacks-start (range 100))]
      ;;(pp/pprint blacks-final))
      (println (format "Black tiles after 100 iterations: %d" (count blacks-final))))))
