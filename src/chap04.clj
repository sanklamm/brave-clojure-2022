(ns chap04 
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; some unfinished scribbles

(seq "foo")
(cons 1 '(2 3))
(.toString [1 2 3])
(map (juxt odd? even? inc dec) [1 2 3 4 5])

;; differences between seqs and lists (because they look the same)
(list 1 2 3)
;; => (1 2 3)

(seq [1 2 3])
;; => (1 2 3)
(class (list 1 2 3))
;; => clojure.lang.PersistentList
(class [1 2 3])
;; => clojure.lang.PersistentVector
(class (seq [1 2 3]))
;; => clojure.lang.PersistentVector$ChunkedSeq

;; reimplement map, filter, some using reduce

(take 10 (repeatedly (fn [] (rand-int 10))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vampire Data Analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def file "suspects.csv")
(defn file->reader [filename]
  (->> filename
       io/resource
       io/reader))

(def input (slurp (file->reader file)))

(def vamp-keys [:name :glitter-index])
(defn str->int [str]
  (Integer/parseInt str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert [vamp-key value]
  ((get conversions vamp-key) value))


(defn mapify [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))
 
(def suspects
  (->> input
       str/split-lines
       (map #(str/split % #","))
       mapify))

(->> input
     str/split-lines
     (map #(str/split % #","))
     mapify
     (glitter-filter 2))

;;;;;;;;;;
;; ex01 ;;
;;;;;;;;;;

;; I like the threading macro for data pipelines

(->> input
     str/split-lines
     (map #(str/split % #","))
     mapify
     (glitter-filter 3)
     (map :name))  ; this is the relevant part
;; => ("Edward Cullen" "Jacob Black" "Carlisle Cullen")

;;;;;;;;;;
;; ex02 ;;
;;;;;;;;;;

(defn append [m s]
  (concat m (mapify (conj [] (str/split s #",")))))

(append suspects "Eli,50")
;; => ({:name "Edward Cullen", :glitter-index 10}
;;     {:name "Bella Swan", :glitter-index 0}
;;     {:name "Charlie Swan", :glitter-index 0}
;;     {:name "Jacob Black", :glitter-index 3}
;;     {:name "Carlisle Cullen", :glitter-index 6}
;;     {:name "Eli", :glitter-index 50})

;;;;;;;;;;
;; ex03 ;;
;;;;;;;;;;

(def validation-fns-1 {:name string? :glitter-index integer?})
(def validation-fns-2 {:name string? :glitter-index odd?})
(def validation-fns-3 {:name string? :glitter-index even?})


(defn validate [validations record]
  (every? true? (map
                 (fn [[key validation-fn]] (validation-fn (key record))) ; f
                 validations)))                                         ; coll

(validate validation-fns-1 {:name "Eli" :glitter-index 50})
;; => true

(defn append-with-validation [m s v]
  (let [suspect (mapify (conj [] (str/split s #",")))]
    (if (validate v (first suspect))
      (concat m suspect)
      m)))

(append-with-validation suspects "Eli,5" validation-fns-1)
;; => ({:name "Edward Cullen", :glitter-index 10}
;;     {:name "Bella Swan", :glitter-index 0}
;;     {:name "Charlie Swan", :glitter-index 0}
;;     {:name "Jacob Black", :glitter-index 3}
;;     {:name "Carlisle Cullen", :glitter-index 6}
;;     {:name "Eli", :glitter-index 5})

(append-with-validation suspects "Eli,5" validation-fns-2)
;; => ({:name "Edward Cullen", :glitter-index 10}
;;     {:name "Bella Swan", :glitter-index 0}
;;     {:name "Charlie Swan", :glitter-index 0}
;;     {:name "Jacob Black", :glitter-index 3}
;;     {:name "Carlisle Cullen", :glitter-index 6}
;;     {:name "Eli", :glitter-index 5})

(append-with-validation suspects "Eli,5" validation-fns-3)
;; => ({:name "Edward Cullen", :glitter-index 10}
;;     {:name "Bella Swan", :glitter-index 0}
;;     {:name "Charlie Swan", :glitter-index 0}
;;     {:name "Jacob Black", :glitter-index 3}
;;     {:name "Carlisle Cullen", :glitter-index 6})

;;;;;;;;;;
;; ex04 ;;
;;;;;;;;;;


(defn map->csv [m]
  (->> m
     (map #(vals %))
     (map #(str/join "," %))
     (str/join "\n")))

(map->csv suspects)
;; => "Edward Cullen,10\nBella Swan,0\nCharlie Swan,0\nJacob Black,3\nCarlisle Cullen,6"

