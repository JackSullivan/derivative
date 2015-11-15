(ns derivative.core)

;; thinking too hard
;; (defmulti derivative (if ))

;; (defmethod derivative java.lang.Long 0)
;; (defmethod derivative clojure.lang.Keyword )

;; (defprotocol Term
;;   (show [self])
;;   (derivative [self var]))

;; (extend-protocol Term
;;   java.lang.Long
;;   (show [self] (str self))
;;   (derivative [self _] 0)
;;   clojure.lang.Keyword
;;   (show [self] (name self))
;;   (derivative [self var] (if (= self var) 1 0)))

(defmulti differentiate (fn [[expr _]] (if (seq? expr) (first expr) (type expr))))

(defmethod differentiate Long [[expr _]] 0)
(defmethod differentiate clojure.lang.Keyword [[expr var]] (if (= expr var) 1 0))
(defmethod differentiate '+
  [[expr var]]
  (cons (first expr) (map #(differentiate [% var]) (rest expr))))
(defmethod differentiate '*
  [[expr var]]
  (let [idx-expr (zip-index (rest expr))
        remove-idx (fn [idx i-e] (filter (comp not (partial = idx) first) i-e))]
    (cons '+ (map-indexed (fn [idx itm]
                            (cons '* (cons (differentiate [itm var]) (map second (remove-idx idx idx-expr))))) (rest expr)))))


(differentiate ['(+ :x 4 :y 3) :x])

(differentiate ['(* :x :x) :x])
