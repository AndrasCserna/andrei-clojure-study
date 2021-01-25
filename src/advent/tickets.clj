;; Advent of Code challenge, 2020 day 16
(ns advent.tickets
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp]))

(defn parse-tickets
  [filename]
  (with-open [input (clojure.java.io/reader filename)]
    (let [split-line (fn [line] (vec (map #(Integer/parseInt %) (str/split line #","))))]
      (vec (map split-line (line-seq input))))))

(defn parse-field-def
  [line]
  (let [field-def (first (re-seq #"([\w\s]+): ([\s\S]+)" line))
        name (nth field-def 1)
        ranges-def (re-seq #"([\d]+)-([\d]+)" (nth field-def 2))
        ranges (map #(vec [(Integer/parseInt (nth % 1)) (Integer/parseInt (nth % 2))]) ranges-def)]
    {name ranges}))

;; every number in [num] falls into at least one of the allowed ranges?
(defn valid-ranges?
  [ranges nums]
  (every? (fn [num] (some (fn [range] (<= (first range) num (second range))) ranges)) nums))

;; every [index]-th element in [tickets] falls into at least one of the allowed ranges?
(defn valid-field?
  [ranges tickets index]
  (valid-ranges? ranges (map #(nth % index) tickets)))

(defn map-values
  [f m]
  (into {} (for [[k v] m] [k (f k v)])))

(defn assignment-step
  [assigned unassigned]
  (let [certain-assignment (first (filter #(= 1 (count (second %))) unassigned))]
    (if certain-assignment
      ;; there exists a field with only one possible field index remaining
      (let [field-name (first certain-assignment)
            field-num (first (second certain-assignment))]
        ;;(println (format "%s -> %d" field-name field-num))
        [
         ;; expand the fixed assignments with the new information
         (into assigned [[field-name field-num]])

         ;; remove this field name and index from the remaining possibilities
         (->> unassigned
              (remove (fn [[k v]] (= field-name k)))
              (map-values (fn [k v] (remove #(= % field-num) v))))
         (into {})
         ])

      ;; no obvious assignment possible - return original input
      [assigned unassigned])
    ))


(defn -main []
  (let [tickets (vec (parse-tickets "day16-tickets.txt"))
        my-ticket (first (parse-tickets "day16-myticket.txt"))
        field-defs (with-open [input (clojure.java.io/reader "day16-fields.txt")]
                     (into {} (map #(parse-field-def %) (line-seq input))))]

    ;;(pp/pprint my-ticket)
    ;;(pp/pprint tickets)
    ;;(pp/pprint field-defs)

    (let [all-ranges (apply concat (vals field-defs))
          valid-tickets (filter #(valid-ranges? all-ranges %) tickets)
          invalid-values (filter #(not (valid-ranges? all-ranges [%])) (flatten tickets))]

      ;; output for part 1
      (println (format "Ticket scanning error rate: %d" (apply + invalid-values)))

      (let [
            ;; initial value of all possible field -> index mappings
            pmap-initial (zipmap (keys field-defs) (repeat (range (count field-defs))))
            ;; remove possibilities where ticket data clashes with allowed field ranges
            pmap-tickets (map-values (fn [name idxlist] (filter #(valid-field? (field-defs name) valid-tickets %) idxlist)) pmap-initial)
            ;; iterate fixing fields while there is an obvious assignment
            assignments-final (loop [assigned {}
                                     unassigned pmap-tickets]
                                (let [[new-assigned new-unassigned] (assignment-step assigned unassigned)]
                                  (if (and (= assigned new-assigned)) assigned (recur new-assigned new-unassigned))))
            ;; departure-related fields on my ticket
            my-departure (->> (keys field-defs)
                              (filter #(str/starts-with? % "departure"))
                              (map (fn [name] {name, (nth my-ticket (assignments-final name))}))
                              (into {}))
            ]
        ;;(pp/pprint pmap-tickets)
        ;;(pp/pprint assignments-final)
        ;;(pp/pprint my-departure)
        (println (format "My departure factor: %d" (apply * (vals my-departure))))

        ))))
