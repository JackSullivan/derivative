(ns derivative.core
  (:require [clojure.math.numeric-tower :as math]))
;;  (:require [derivative.useful :refer [zip-index]]))


;; Utility functions, these should go somewhere else eventually
(defn zip-index
  "returns a coll where each element is a vector of [index (nth coll index)]"
  [coll] (map-indexed vector coll))

(defn dichotomize
  "returns [coll-t coll-f] s.t. (and (every? pred coll-t) (every? (partial not pred) coll-f))"
  [pred coll]
  [(filter pred coll) (filter (comp not pred) coll)])

(defn expression-type [expr] (if (seq? expr) (first expr) (type expr)))

;; symbolic differentiation function, takes nested s-expressions that represent an equation and a variable to differentiate by.
(defmulti differentiate (fn [[expr _]] (expression-type expr)))

(defmethod differentiate Long [[expr _]] 0)
(defmethod differentiate clojure.lang.Keyword [[expr var]] (if (= expr var) 1 0))
(defmethod differentiate '+
  [[expr var]]
  (cons (first expr) (map #(differentiate [% var]) (rest expr))))
(defmethod differentiate '-
  [[expr var]]
  (cons (first expr) (map #(differentiate [% var]) (rest expr))))
(defmethod differentiate '*
  [[expr var]]
  (let [idx-expr (zip-index (rest expr))
        remove-idx (fn [idx i-e] (filter (comp not (partial = idx) first) i-e))]
    (cons '+ (map-indexed (fn [idx itm]
                            (cons '* (cons (differentiate [itm var]) (map second (remove-idx idx idx-expr))))) (rest expr)))))
(defmethod differentiate 'math/expt
  [[[_ base exponent] var]]
  (list '* exponent ('math/expt base (- exponent 1))))

;; simplifies nested s-expressions such that the simplified result is equivalent to the original value. Should be called after differentiate to clean up
(defmulti simplify expression-type)

(defmethod simplify Long [v] v)
(defmethod simplify clojure.lang.Keyword [k] k)

; todo: A lot of redunancy here - do something better
(defmethod simplify '+
  [expr]
  (let [elems (map simplify (rest expr))
        [lits vars-and-ops] (dichotomize (comp (partial = Long) type) elems)
        [vars ops] (dichotomize (comp (partial = clojure.lang.Keyword) type) vars-and-ops)
        num-lit (simplify (reduce + lits))
        cons-vars (map (fn [[var cnt]] (simplify (list '* var cnt))) (frequencies vars))
        simple-exprs (concat (if (= 0 num-lit) nil (list num-lit)) cons-vars ops)]
    (if (= 1 (count simple-exprs)) simple-exprs (cons '+ simple-exprs))))

(defmethod simplify '-
  [expr]
  (let [elems (map simplify (rest expr))
        [lits vars-and-ops] (dichotomize (comp (partial = Long) type) elems)
        [vars ops] (dichotomize (comp (partial = clojure.lang.Keyword) type) vars-and-ops)
        num-lit (simplify (reduce - lits))
        cons-vars (map (fn [[var cnt]] (simplify (list '- (list '* var cnt)))) (frequencies vars))
        simple-exprs (concat (if (= 0 num-lit) nil (list num-lit)) cons-vars ops)]
    (if (= 1 (count simple-exprs)) simple-exprs (cons '- simple-exprs))))

(defmethod simplify '*
  [expr]
  (let [elems (map simplify (rest expr))
        [lits vars-and-ops] (dichotomize (comp (partial = Long) type) elems)
        [vars ops] (dichotomize (comp (partial = clojure.lang.Keyword) type) vars-and-ops)
        num-lit (simplify (reduce * lits))
        cons-vars (map (fn [[var cnt]] (simplify (list 'math/expt var cnt))) (frequencies vars))
        simple-exprs (concat (if (= 0 num-lit) nil (list num-lit)) cons-vars ops)]
    (if (= 1 (count simple-exprs)) simple-exprs (cons '* simple-exprs))))

(defmethod simplify 'math/expt
  [[_ base exponent]] (map (fn [e] (cond (= e 0) 1
                                            (= e 1) base
                                            :else (list 'math/expt base e))) (simplify exponent)))
