(ns advent.labyrinth
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]))

(defn abs [n] (max n (- n)))

(defn get-raw
  ([[row col] raw-data]
   (get (get raw-data row []) col nil))

  ([[row col] raw-data not-found]
   (get (get raw-data row []) col not-found)))

(defn get-neighbor-pos
  [[row col]]
  {:up [(- row 1) col], :down [(+ row 1) col], :left [row (- col 1)], :right [row (+ col 1)]})

;; parse portal position and name in the raw data
(defn get-portal-raw
  [[row col] raw-data]

  (if (Character/isLetter ^char (get-raw [row col] raw-data \#))
    (let [pos (into {:self [row col]} (get-neighbor-pos [row col]))
          raw (into {} (map (fn [[k v]] [k (get-raw v raw-data)]) pos))
          is-alpha (into {} (map (fn [[k v]] [k (if v (Character/isLetter ^char v) false)]) raw))
          is-dot (into {} (map (fn [[k v]] [k (= v \.)]) raw))]
      (cond
        (and (is-alpha :left) (is-dot :right)) [(apply str [(raw :left) (raw :self)]) (pos :right)]
        (and (is-alpha :right) (is-dot :left)) [(apply str [(raw :self) (raw :right)]) (pos :left)]
        (and (is-alpha :up) (is-dot :down)) [(apply str [(raw :up) (raw :self)]) (pos :down)]
        (and (is-alpha :down) (is-dot :up)) [(apply str [(raw :self) (raw :down)]) (pos :up)]
        :else nil))
    nil)
  )

;; get a list of portals, with inner/outer type determined by distance from edges
(defn process-portals
  [raw-input]
  (let [size [(count raw-input) (reduce max (map count raw-input))]
        dist-edge (fn [[row col]] (min row col (- (first size) row) (- (second size) col)))
        pos-all (for [row (range (first size)) col (range (second size))] [row col])
        portals (->> pos-all
                     (map #(get-portal-raw % raw-input))
                     (filter #(some? %))
                     (group-by first)
                     (map (fn [[k v]] [k (sort-by dist-edge (map second v))]))
                     (into {}))
        ]
    (->> portals
         (map (fn [[name pos-pair]] [
                                     [[name :outside] (first pos-pair)]
                                     [[name :inside] (second pos-pair)]
                                     ]))
         (apply concat)
         (filter #(some? (second %)))
         )
    ))

;; all single-step movement possibilities from a given tile
(defn walking-moves
  [[row col] dots]
  (->> (get-neighbor-pos [row col])
       (filter (fn [[_ pos]] (contains? dots pos)))
       (map (fn [[_ pos]] {:to pos, :distance 1}))
       (vec)))

;; remove all moves from the map referencing the given list of positions
(defn remove-moves
  [all-moves removed]
  (->> all-moves
       (filter (fn [[pos _]] (not (contains? removed pos))))
       (map (fn [[pos moves]] [pos
                               (->> moves
                                    (filter (fn [move] (not (contains? removed (move :to)))))
                                    (vec)
                                    )
                               ]))
       (into {})
       ))

;; add a new move to the graph
(defn add-moves
  [all-moves pos1 pos2 dist]
  (->> all-moves
       (map (fn [[pos moves]] [pos (cond
                                     (= pos pos1) (conj moves {:to pos2 :distance dist})
                                     (= pos pos2) (conj moves {:to pos1 :distance dist})
                                     :else moves
                                     )]
              ))
       (into {})
       ))

;; remove all dead ends from the list of moves
;; the given list of positions is exempt from removal
(defn trim-moves-1
  [moves protected-pos]
  (let [dead-end (->> moves
                      (filter (fn [[_ moves-pos]] (> 2 (count moves-pos))))
                      (map first)
                      (set))
        removed (set/difference (set dead-end) (set protected-pos))]
    (remove-moves moves removed)))

;; follow a corridor (chain of nodes with exactly 2 neighbors) to the end
(defn follow-corridor
  [from curr moves]
  (loop [from from curr curr]
    (if (not (= 2 (count (moves (curr :to)))))
      ;; not a corridor, stop following
      (into curr {:tiles from})
      ;; take the other move from where we came
      (let [other (->> (moves (curr :to))
                       (filter (fn [move] (not= (last from) (move :to))))
                       (first))]
        (recur (conj from (curr :to)) {:to (other :to) :distance (+ (curr :distance) (other :distance))})))))

;; pick a random corridor (exactly 2 neighbors) and collapse it in the labyrinth graph
(defn trim-moves-2
  [all-moves]
  (let [corr (->> all-moves
                  (filter (fn [[_ pos-moves]] (= 2 (count pos-moves))))
                  (first)
                  )]
    (if (not corr)
      ;; no more corridors
      all-moves
      (let [pos (first corr)
            moves (second corr)
            ;; find both ends of the corridor
            A (follow-corridor [pos] (first moves) all-moves)
            B (follow-corridor [pos] (second moves) all-moves)
            tiles (set (concat (A :tiles) (B :tiles)))
            ]
        ;; remove all corridor nodes, add direct link between 2 endpoints
        (add-moves (remove-moves all-moves tiles) (A :to) (B :to) (+ (A :distance) (B :distance)))
        ))))

;; single BFS step, pick the next step with the lowest total distance
(defn search-step
  [distances moves]
  (let [existing (set (keys distances))
        possible-moves (->> distances
                            (map (fn [[pos dist]]
                                   (->> (moves pos)
                                        (map (fn [step] (into step {
                                                                    ;:from      pos
                                                                    :path      (conj (dist :path) (step :to))
                                                                    :last-step (step :distance)
                                                                    :distance  (+ (dist :distance) (step :distance))
                                                                    }))))
                                   ))
                            (apply concat)
                            (filter (fn [move] (not (contains? existing (move :to)))))
                            )
        ]
    (if (empty? possible-moves)
      distances
      (let [next-move (apply min-key (fn [move] (move :distance)) possible-moves)]
        ;(pp/pprint (dissoc next-move :path :last-step))
        (into distances {(next-move :to) {:distance (next-move :distance) :path (next-move :path)}})))
    )
  )

;; BFS loop, stop when gives terminating predicate is true
;; runs until all possibilities are exhausted if predicate omitted
(defn search
  ([distances moves term]
   (loop [distances distances]
     (let [new-distances (search-step distances moves)]
       ;(pp/pprint new-distances)
       (if (term distances new-distances) new-distances (recur new-distances)))))
  ([distances moves] (search distances moves (fn [old new] (= (count old) (count new))))))

(defn -main []
  (binding [clojure.pprint/*print-right-margin* 1250]
    (let [raw-input (vec (line-seq (clojure.java.io/reader "2019-day20-labyrinth.txt")))
          size [(count raw-input) (reduce max (map count raw-input))]
          pos-all (for [row (range (first size)) col (range (second size))] [row col])
          pos-dots (set (filter (fn [[row col]] (= \. (get-raw [row col] raw-input))) pos-all))
          ;; naive labyrinth graph from dots
          moves (->> pos-dots
                     (map (fn [pos] [pos (walking-moves pos pos-dots)]))
                     (into {}))
          ;; list of portals: [name side] -> [x y]
          portals (->> (process-portals raw-input))
          ;; get portal coords from identifier [name side]
          get-portal (fn [pk] (second (->> portals (filter #(= (first %) pk)) (first))))
          ;; labyrinth graph with useless dead-ends iteratively removed
          reduced-moves-1 (loop [current-moves moves]
                            (let [new-moves (trim-moves-1 current-moves (map second portals))]
                              (if (= (count new-moves) (count current-moves)) new-moves (recur new-moves))))
          ;; labyrinth graph with long corridors collapsed into a single graph edge
          reduced-moves-2 (loop [current-moves reduced-moves-1]
                            (let [new-moves (trim-moves-2 current-moves)]
                              (if (= (count new-moves) (count current-moves)) new-moves (recur new-moves))))
          ;; list of all intra-layer (without using portals) movement possibilities between portals
          ;; "coordinates" in this map are portal names
          layer-moves (->> portals
                           (map (fn [[source spos]]
                                  ;; run search to find all reachable node distances
                                  (let [possible-moves (search {(get-portal source) {:path [spos] :distance 0}} reduced-moves-2)]
                                    (->> portals
                                         (filter #(not= source (first %)))
                                         (map (fn [[target tpos]] {:from source :to target :distance (get (possible-moves tpos) :distance)}))
                                         (filter #(% :distance)))
                                    )))
                           (apply concat)
                           )
          ;; function for generating movement options for a type 1 (singleton) labyrinth
          simple-moves (fn [[name type]]
                         (let [portal-move [name (if (= type :inside) :outside :inside)]
                               valid-portal? (fn [[name type]] (some #(= (first %) [name type]) portals))
                               walking-moves (->> layer-moves
                                                  (filter #(= (% :from) [name type])))
                               ]
                           (if (valid-portal? portal-move) (conj walking-moves {:from [name type] :to portal-move :distance 1}) walking-moves)
                           ))

          ;; limit maximum traversal depth - search slows down a lot without it
          max-layer 30
          ;; function for generating movement options for a type 2 (recursive) labyrinth
          recursive-moves (fn [[name type layer]]
                            (let [portal-move [name
                                               (if (= type :inside) :outside :inside)
                                               (if (= type :inside) (inc layer) (dec layer))]
                                  valid-portal? (fn [[name type layer]] (and
                                                                          (<= 0 layer max-layer)
                                                                          (some #(= (first %) [name type]) portals)
                                                                          ))
                                  walking-moves (->> layer-moves
                                                     (filter #(= (% :from) [name type]))
                                                     (map (fn [move] (into move {:from (conj (move :from) layer) :to (conj (move :to) layer)})))
                                                     )
                                  ]
                              (if (valid-portal? portal-move) (conj walking-moves {:from [name type layer] :to portal-move :distance 1}) walking-moves)
                              ))

          ]

      ;(pp/pprint raw-input)
      ;(pp/pprint size)
      ;(pp/pprint portals)

      ;(pp/pprint moves)
      ;(pp/pprint reduced-moves-1)
      ;(pp/pprint reduced-moves-2)

      ;(pp/pprint layer-moves)

      (let [part1-start {["AA" :outside] {:distance 0 :path [["AA" :outside]]}}
            part1-traversal (search part1-start simple-moves)
            part1-solution (part1-traversal ["ZZ" :outside])
            ]
        ;;(pp/pprint part1-solution)
        (println (format "Normal labyrinth: going from AA to ZZ takes %d steps" (part1-solution :distance))))

      (let [part2-start {["AA" :outside 0] {:distance 0 :path [["AA" :outside 0]]}}
            part2-stop (fn [_ new] (contains? (set (keys new)) ["ZZ" :outside 0]))
            part2-traversal (search part2-start recursive-moves part2-stop)
            part2-solution (part2-traversal ["ZZ" :outside 0])
            ]
        ;;(pp/pprint part2-solution)
        (println (format "Recursive labyrinth: going from AA[0] to ZZ[0] takes %d steps" (part2-solution :distance))))

      )))