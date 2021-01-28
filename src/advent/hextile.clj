(ns advent.hextile
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:require [clojure.set :as set]))

(defn count-steps
  [line]
  ;; count the number of steps in the 3 east-bound cardinal directions
  (let [steps (re-seq #"[sn]?[we]" line)]
    (reduce (fn [aggr step] (into aggr (case step
                                         "e" {:E (inc (aggr :E))}
                                         "w" {:E (dec (aggr :E))}
                                         "ne" {:NE (inc (aggr :NE))}
                                         "sw" {:NE (dec (aggr :NE))}
                                         "se" {:SE (inc (aggr :SE))}
                                         "nw" {:SE (dec (aggr :SE))}
                                         ))) {:E 0, :NE 0, :SE 0} steps)
    ))

(defn simplify-steps
  [steps]
  ;; make coordinates unique by removing redundant degree of freedom
  ;; SE = E - NE, return vec of [#steps E, #steps NE]
  [(+ (get steps :E) (get steps :SE))
   (- (get steps :NE) (get steps :SE))]
  )

(defn count-map
  [m]
  ;; count all the occurrences of unique elements into a map
  (reduce (fn [counts elem] (conj counts
                                  {elem (inc (or (get counts elem) 0))}
                                  )) {} m))

(def neighbor-offsets [[1 0] [-1 0] [0 1] [0 -1] [1 -1] [-1 1]])
(defn neighbors
  [tile]
  ;; get list of neighboring tiles
  (map #(mapv + tile %) neighbor-offsets)
  )

(defn bounds
  [tile-list]
  ;; calculate coordinate bounds of listed tiles
  (let [x (map #(first %) tile-list)
        y (map #(second %) tile-list)]
    [
     (apply min x)
     (apply max x)
     (apply min y)
     (apply max y)
     ]))

(defn evolve-state
  [tile-set]
  ;; calculate list of all tiles with coordinates +/- 1 (inclusive) of existing black tiles
  (let [area-bounds (mapv + [-1 2 -1 2] (bounds tile-set))
        check-tiles (set (for [x (range (nth area-bounds 0) (nth area-bounds 1)) y (range (nth area-bounds 2) (nth area-bounds 3))] [x y]))]
    (set (filter (fn [tile]
                   ;; calculate next state of tile
                   (let [is-black (contains? tile-set tile)
                         num-blacks (count (set/intersection tile-set (set (neighbors tile))))]
                     (cond
                       (and (not is-black) (= 2 num-blacks)) true
                       (and is-black (< 0 num-blacks 3)) true
                       :else false
                       )
                     )) check-tiles)
         )))

(defn -main []
  (let [tiles (with-open [input (clojure.java.io/reader "day24-tiles.txt")] (vec (line-seq input)))
        tiles-unique (map #(simplify-steps (count-steps %)) tiles)
        blacks-start (set (keys (filter (fn [[k v]] (odd? v)) (count-map tiles-unique))))]

    ;;(pp/pprint tiles-unique)
    ;;(pp/pprint (count-map tiles-unique))
    ;;(pp/pprint blacks-start)
    (println (format "Black starting tiles: %d" (count blacks-start)))

    (let [blacks-final (reduce (fn [state num] (evolve-state state)) blacks-start (range 100))]
      ;;(pp/pprint blacks-final))
      (println (format "Black tiles after 100 iterations: %d" (count blacks-final))))
    ))