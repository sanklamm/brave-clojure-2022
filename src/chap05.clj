(ns chap05)

;;;;;;;;;;
;; ex01 ;;
;;;;;;;;;;

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(defn attr [kw]
  (comp kw :attributes))

((attr :intelligence) character)

;;;;;;;;;;
;; ex02 ;;
;;;;;;;;;;

(defn my-comp
  ([] identity)
  ([f] f)
  ([f g]
   (fn [& args]
     (f (apply g args))))
  ([f g & fs]
   (reduce my-comp (cons f(cons g(seq fs)))))) ; could be simplified with list*

((comp) 1)
;; => 1
((my-comp) 1)
;; => 1
((comp inc) 1)
;; => 2
((my-comp inc) 1)
;; => 2
((comp inc inc) 1)
;; => 3
((my-comp inc inc) 1)
;; => 3
((comp inc inc inc dec *) 2 5)
;; => 12
((my-comp inc inc inc dec *) 2 5)
;; => 12

;;;;;;;;;;
;; ex03 ;;
;;;;;;;;;;

(defn my-assoc-in [m [k & ks] v]
  (if ks
    (assoc m k (my-assoc-in (get m k) ks v)) ; recursive part
    (assoc m k v)))


(assoc-in {} [:foo :bar :baz] "bat")
;; => {:foo {:bar {:baz "bat"}}}
(assoc {} :baz "bat")
;; => {:baz "bat"}
(my-assoc-in {} [:foo] "bar")
;; => {:foo "bar"}
(my-assoc-in {} [:foo :bar] "baz")
;; => {:foo {:bar "baz"}}
(my-assoc-in {:this "that"} [:foo :bar :baz] "bat")
;; => {:this "that", :foo {:bar {:baz "bat"}}}


;;;;;;;;;;
;; ex04 ;;
;;;;;;;;;;

(def my-map {:this "that", :foo {:bar {:baz 1}}})
(update-in my-map [:foo :bar :baz] inc)
;; => {:this "that", :foo {:bar {:baz 2}}}
(update-in my-map [:foo :bar :baz] (juxt inc dec odd? even?))
;; => {:this "that", :foo {:bar {:baz [2 0 true false]}}}
(update-in my-map [:foo :bar :baz] + 2 3)
;; => {:this "that", :foo {:bar {:baz 6}}}


;;;;;;;;;;
;; ex05 ;;
;;;;;;;;;;

(defn my-update-in [m [k & ks] f & args]
  (if ks
    (assoc m k (apply my-update-in (get m k) ks f args)) ; recursive part
    (assoc m k (apply f (k m) args))))

(update-in {:foo 2} [:foo] inc)
;; => {:foo 3}
(update-in {:foo 2} [:foo] * 3 4)
;; => {:foo 24}
(my-update-in {:foo 2} [:foo] inc)
;; => {:foo 3}
(my-update-in {:foo 2} [:foo] * 3 4)
;; => {:foo 24}
(my-update-in my-map [:foo :bar :baz] inc)
;; => {:this "that", :foo {:bar {:baz 2}}}
(my-update-in my-map [:foo :bar :baz] (juxt inc dec odd? even?))
;; => {:this "that", :foo {:bar {:baz [2 0 true false]}}}
(my-update-in my-map [:foo :bar :baz] + 2 3);; => {:this "that", :foo {:bar {:baz 6}}}
