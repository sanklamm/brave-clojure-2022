(ns chap07)

;;;;;;;;;;;
;; ex 01 ;;
;;;;;;;;;;;

(eval (list (read-string "prn") "Sebastian" "Blade Runner"))

;;;;;;;;;;;
;; ex 02 ;;
;;;;;;;;;;;

(defmacro simple-unfixxer
  "Converts the given example to prefix notation."
  [[first-op op-plus second-op op-times third-op op-minus forth-op]] 
  (list op-minus
        (list op-plus
         first-op
         (list op-times second-op third-op))
        forth-op))

(macroexpand '(simple-unfixxer (1 + 3 * 4 - 5)))
;; => (- (+ 1 (* 3 4)) 5)

;; real solution

(def ops '[- + * /])
(def op-rank (zipmap ops (iterate inc 1)))
(def op? op-rank)

(defn unfix
  "Takes a list of infix notation and produces prefix notation."
  [[a b & [c d & rst]]]
  (cond
    (op? b) (if (and d (< (op-rank b 0) (op-rank d 0)))
              (recur (list a b (unfix (list* c d rst))))
              (recur (list* (list b a c) d rst)))
    :else a))

(unfix '(1 + 3 * 4 - 5))
;; => (+ 1 (- (* 3 4) 5))
