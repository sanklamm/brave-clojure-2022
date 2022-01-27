(ns chap03 
  (:require
    [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;
;; Hobbit Model ;;
;;;;;;;;;;;;;;;;;;

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part [part]
  {:name (str/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(symmetrize-body-parts asym-hobbit-body-parts)
;; => [{:name "head", :size 3}
;;     {:name "left-eye", :size 1}
;;     {:name "right-eye", :size 1}
;;     {:name "left-ear", :size 1}
;;     {:name "right-ear", :size 1}
;;     {:name "mouth", :size 1}
;;     {:name "nose", :size 1}
;;     {:name "neck", :size 2}
;;     {:name "left-shoulder", :size 3}
;;     {:name "right-shoulder", :size 3}
;;     {:name "right-upper-arm", :size 3}
;;     {:name "left-upper-arm", :size 3}
;;     {:name "chest", :size 10}
;;     {:name "back", :size 10}
;;     {:name "left-forearm", :size 3}
;;     {:name "right-forearm", :size 3}
;;     {:name "abdomen", :size 6}
;;     {:name "left-kidney", :size 1}
;;     {:name "right-kidney", :size 1}
;;     {:name "left-hand", :size 2}
;;     {:name "right-hand", :size 2}
;;     {:name "right-knee", :size 2}
;;     {:name "left-knee", :size 2}
;;     {:name "right-thigh", :size 4}
;;     {:name "left-thigh", :size 4}
;;     {:name "right-lower-leg", :size 3}
;;     {:name "left-lower-leg", :size 3}
;;     {:name "right-achilles", :size 1}
;;     {:name "left-achilles", :size 1}
;;     {:name "right-foot", :size 2}
;;     {:name "left-foot", :size 2}]

(defn better-symmetrize-body-parts [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))


;;;;;;;;;;
;; ex01 ;;
;;;;;;;;;;

(def meetup-name "Clojure for the Brave and True")
(str "Hello to " meetup-name " - "2)
;; => "Hello to Clojure for the Brave and True - 2"

;; vectors behave like Java arrays
(conj (vec '(1 2 3 4)) 5)
;; => [1 2 3 4 5]

;; a list is a singly linked list
(conj '(1 2 3 4) 5)
;; => (5 1 2 3 4)

;; maps are fun
(def my-map {:a 1 :b [2 3] :c 4 :e {:f 99}})
(:a my-map)
;; => 1
(my-map :b)
;; => [2 3]
(:d my-map)
;; => nil
(:d my-map "default val")
;; => "default val"
(get-in my-map [:e :f])
;; => 99

;; sets are very useful as well
(def calculated-moves [:left :right :left :up :right :left :right])
;; benifits include fast lookup and uniquifying a collection
(def unique-moves (into [] (set calculated-moves)))
unique-moves
;; => [:up :right :left]


;;;;;;;;;;
;; ex02 ;;
;;;;;;;;;;

(defn add-100 [num]
  (+ 100 num))

(add-100 23)
;; => 123


;;;;;;;;;;
;; ex03 ;;
;;;;;;;;;;

(defn dec-maker [x]
  #(- % x))

(def dec9 (dec-maker 9))
(dec9 10)
;; => 1


;;;;;;;;;;
;; ex04 ;;
;;;;;;;;;;

(defn mapset [f coll]
  (into #{} (map f coll)))
(mapset inc [1 1 2 2])
;; => #{3 2}


;;;;;;;;;;
;; ex05 ;;
;;;;;;;;;;



(def asym-alien-body-parts [{:name "head" :size 3}
                            {:name "1-eye" :size 1}
                            {:name "1-ear" :size 1}
                            {:name "mouth" :size 1}
                            {:name "nose" :size 1}
                            {:name "neck" :size 2}
                            {:name "1-shoulder" :size 3}
                            {:name "1-upper-arm" :size 3}
                            {:name "chest" :size 10}
                            {:name "back" :size 10}
                            {:name "1-forearm" :size 3}
                            {:name "abdomen" :size 6}
                            {:name "1-kidney" :size 1}
                            {:name "1-hand" :size 2}
                            {:name "1-knee" :size 2}
                            {:name "1-thigh" :size 4}
                            {:name "1-lower-leg" :size 3}
                            {:name "1-achilles" :size 1}
                            {:name "1-foot" :size 2}])

(defn multiply-alien-parts-by-5 [part]
  (for [x (range 2 6)
        :let [matched-part {:name (str/replace (:name part) #"^1-" (str x "-"))
                            :size (:size part)}]]
    matched-part))

(defn complete-alien-body-parts [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set (concat [part] (multiply-alien-parts-by-5 part)))))
          []
          asym-body-parts))

(complete-alien-body-parts asym-alien-body-parts)
;; => [{:name "head", :size 3}
;;     {:name "5-eye", :size 1}
;;     {:name "3-eye", :size 1}
;;     {:name "2-eye", :size 1}
;;     {:name "1-eye", :size 1}
;;     {:name "4-eye", :size 1}
;;     {:name "3-ear", :size 1}
;;     {:name "1-ear", :size 1}
;;     {:name "2-ear", :size 1}
;;     {:name "4-ear", :size 1}
;;     {:name "5-ear", :size 1}
;;     {:name "mouth", :size 1}
;;     {:name "nose", :size 1}
;;     {:name "neck", :size 2}
;;     {:name "5-shoulder", :size 3}
;;     {:name "4-shoulder", :size 3}
;;     {:name "2-shoulder", :size 3}
;;     {:name "1-shoulder", :size 3}
;;     {:name "3-shoulder", :size 3}
;;     {:name "4-upper-arm", :size 3}
;;     {:name "5-upper-arm", :size 3}
;;     {:name "3-upper-arm", :size 3}
;;     {:name "2-upper-arm", :size 3}
;;     {:name "1-upper-arm", :size 3}
;;     {:name "chest", :size 10}
;;     {:name "back", :size 10}
;;     {:name "2-forearm", :size 3}
;;     {:name "4-forearm", :size 3}
;;     {:name "1-forearm", :size 3}
;;     {:name "5-forearm", :size 3}
;;     {:name "3-forearm", :size 3}
;;     {:name "abdomen", :size 6}
;;     {:name "2-kidney", :size 1}
;;     {:name "4-kidney", :size 1}
;;     {:name "5-kidney", :size 1}
;;     {:name "3-kidney", :size 1}
;;     {:name "1-kidney", :size 1}
;;     {:name "5-hand", :size 2}
;;     {:name "2-hand", :size 2}
;;     {:name "4-hand", :size 2}
;;     {:name "1-hand", :size 2}
;;     {:name "3-hand", :size 2}
;;     {:name "3-knee", :size 2}
;;     {:name "1-knee", :size 2}
;;     {:name "5-knee", :size 2}
;;     {:name "2-knee", :size 2}
;;     {:name "4-knee", :size 2}
;;     {:name "3-thigh", :size 4}
;;     {:name "2-thigh", :size 4}
;;     {:name "5-thigh", :size 4}
;;     {:name "4-thigh", :size 4}
;;     {:name "1-thigh", :size 4}
;;     {:name "1-lower-leg", :size 3}
;;     {:name "4-lower-leg", :size 3}
;;     {:name "5-lower-leg", :size 3}
;;     {:name "3-lower-leg", :size 3}
;;     {:name "2-lower-leg", :size 3}
;;     {:name "2-achilles", :size 1}
;;     {:name "5-achilles", :size 1}
;;     {:name "3-achilles", :size 1}
;;     {:name "4-achilles", :size 1}
;;     {:name "1-achilles", :size 1}
;;     {:name "2-foot", :size 2}
;;     {:name "5-foot", :size 2}
;;     {:name "3-foot", :size 2}
;;     {:name "1-foot", :size 2}
;;     {:name "4-foot", :size 2}]




;;;;;;;;;;
;; ex06 ;;
;;;;;;;;;;

(defn multiply-alien-parts [part multiplier]
  (for [x (range 2 (inc multiplier))
        :let [matched-part {:name (str/replace (:name part) #"^1-" (str x "-"))
                            :size (:size part)}]]
    matched-part))

(defn complete-alien-body-parts-with-multiplier [asym-body-parts multiplier]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set (concat [part] (multiply-alien-parts part multiplier)))))
          []
          asym-body-parts))

(complete-alien-body-parts-with-multiplier asym-alien-body-parts 7)
;; => [{:name "head", :size 3}
;;     {:name "5-eye", :size 1}
;;     {:name "3-eye", :size 1}
;;     {:name "2-eye", :size 1}
;;     {:name "7-eye", :size 1}
;;     {:name "1-eye", :size 1}
;;     {:name "6-eye", :size 1}
;;     {:name "4-eye", :size 1}
;;     {:name "3-ear", :size 1}
;;     {:name "7-ear", :size 1}
;;     {:name "6-ear", :size 1}
;;     {:name "1-ear", :size 1}
;;     {:name "2-ear", :size 1}
;;     {:name "4-ear", :size 1}
;;     {:name "5-ear", :size 1}
;;     {:name "mouth", :size 1}
;;     {:name "nose", :size 1}
;;     {:name "neck", :size 2}
;;     {:name "5-shoulder", :size 3}
;;     {:name "4-shoulder", :size 3}
;;     {:name "7-shoulder", :size 3}
;;     {:name "2-shoulder", :size 3}
;;     {:name "1-shoulder", :size 3}
;;     {:name "6-shoulder", :size 3}
;;     {:name "3-shoulder", :size 3}
;;     {:name "4-upper-arm", :size 3}
;;     {:name "6-upper-arm", :size 3}
;;     {:name "5-upper-arm", :size 3}
;;     {:name "3-upper-arm", :size 3}
;;     {:name "7-upper-arm", :size 3}
;;     {:name "2-upper-arm", :size 3}
;;     {:name "1-upper-arm", :size 3}
;;     {:name "chest", :size 10}
;;     {:name "back", :size 10}
;;     {:name "7-forearm", :size 3}
;;     {:name "2-forearm", :size 3}
;;     {:name "4-forearm", :size 3}
;;     {:name "1-forearm", :size 3}
;;     {:name "6-forearm", :size 3}
;;     {:name "5-forearm", :size 3}
;;     {:name "3-forearm", :size 3}
;;     {:name "abdomen", :size 6}
;;     {:name "6-kidney", :size 1}
;;     {:name "7-kidney", :size 1}
;;     {:name "2-kidney", :size 1}
;;     {:name "4-kidney", :size 1}
;;     {:name "5-kidney", :size 1}
;;     {:name "3-kidney", :size 1}
;;     {:name "1-kidney", :size 1}
;;     {:name "5-hand", :size 2}
;;     {:name "7-hand", :size 2}
;;     {:name "6-hand", :size 2}
;;     {:name "2-hand", :size 2}
;;     {:name "4-hand", :size 2}
;;     {:name "1-hand", :size 2}
;;     {:name "3-hand", :size 2}
;;     {:name "3-knee", :size 2}
;;     {:name "6-knee", :size 2}
;;     {:name "1-knee", :size 2}
;;     {:name "5-knee", :size 2}
;;     {:name "2-knee", :size 2}
;;     {:name "7-knee", :size 2}
;;     {:name "4-knee", :size 2}
;;     {:name "3-thigh", :size 4}
;;     {:name "6-thigh", :size 4}
;;     {:name "2-thigh", :size 4}
;;     {:name "7-thigh", :size 4}
;;     {:name "5-thigh", :size 4}
;;     {:name "4-thigh", :size 4}
;;     {:name "1-thigh", :size 4}
;;     {:name "1-lower-leg", :size 3}
;;     {:name "7-lower-leg", :size 3}
;;     {:name "4-lower-leg", :size 3}
;;     {:name "5-lower-leg", :size 3}
;;     {:name "3-lower-leg", :size 3}
;;     {:name "2-lower-leg", :size 3}
;;     {:name "6-lower-leg", :size 3}
;;     {:name "2-achilles", :size 1}
;;     {:name "5-achilles", :size 1}
;;     {:name "6-achilles", :size 1}
;;     {:name "3-achilles", :size 1}
;;     {:name "4-achilles", :size 1}
;;     {:name "7-achilles", :size 1}
;;     {:name "1-achilles", :size 1}
;;     {:name "6-foot", :size 2}
;;     {:name "2-foot", :size 2}
;;     {:name "5-foot", :size 2}
;;     {:name "3-foot", :size 2}
;;     {:name "1-foot", :size 2}
;;     {:name "4-foot", :size 2}
;;     {:name "7-foot", :size 2}]
