(ns chap08)

`+
;; => clojure.core/+

;; What is the difference between defn and defmacro?

(defmacro twice-macro [e] `(do ~e ~e))
(defn twice-fn [e] `(do ~e ~e))
(twice-macro (println "foo"))
;; => nil
(twice-fn (println "bar"))
;; => (do nil nil)

(reduce + (map inc [1 2 3]))
;; => 9
;;(-> x a b c)
;; (c (b (a x)))

(defmacro code-critic [bad good]
  `(do (println "Soo Bad:"
                (quote ~bad))
       (println "Very good:"
                (quote ~good))))

(code-critic (1 + 2) (+ 2 1))

(defn criticize-code
  [criticism code]
  `(println ~criticism (quote ~code)))

(defmacro code-critic-2
  [bad good]
  `(do ~(criticize-code "Amazingly bad:" bad)
       ~(criticize-code "Amazing:" good)))


(code-critic-2 (1 + 2) (+ 2 1))

(defn infix
  [infixed]
  (list (second infixed) (first infixed) (last infixed)))

(infix (1 + 1))

;;;;;;;;;;;;;;;;;;;
;; prerequisites ;;
;;;;;;;;;;;;;;;;;;;

(def order-details
  {:name "Mitchard Blimmons"
   :email "mitchard@blimmonsgmail.com"})

(def order-details-validations
  {:name
   ["Please enter a name" not-empty]

   :email
   ["Please enter an email address" not-empty

    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(error-messages-for "" ["Please enter a name" not-empty])

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(validate order-details order-details-validations)

(let [errors (validate order-details order-details-validations)]
  (if (empty? errors)
    (println :success)
    (println :failure errors)))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(defn render [& items]
  (println items))

(if-valid order-details order-details-validations errors
          (render :success)
          (render :failure errors))

;;;;;;;;;;;
;; ex 01 ;;
;;;;;;;;;;;

(defmacro when-valid
  [to-validate validations & body]
  `(let [errors# (validate ~to-validate ~validations)]
     (if (empty? errors#)
       (do
         ~@body))))

(when-valid order-details order-details-validations
            (println "It's a success!")
            (render :success))


;;;;;;;;;;;
;; ex 02 ;;
;;;;;;;;;;;

(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defmacro or-2
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (or ~@next)))))

(or)
;; => nil
(or true)
;; => true

(or false)
;; => false

(or true false)
;; => true

(or false true)
;; => true


(or-2)
;; => nil
(or-2 true)
;; => true

(or-2 false)
;; => false

(or-2 true false)
;; => true

(or-2 false true)
;; => true



;;;;;;;;;;;
;; ex 03 ;;
;;;;;;;;;;;


(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(defmacro defattrs
  [& pairs]
  `(do
     ~@(map
        (fn [[fn-name attr]] `(def ~fn-name (comp ~attr :attributes)))
        (partition 2 pairs))))

(partition 2 [1 2 3 4 5 6])
;; => ((1 2) (3 4) (5 6))

(defattrs c-int :intelligence
  c-str :strength
  c-dex :dexterity)

(c-str character) 
