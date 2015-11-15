Derivative
==========

Derivative is a library that aims to express some simple mathematical concepts in computable terms. At the moment that entails basic automatic differentiation.

```clojure
  (simplify (differentiate ['(+ (* 4 (math/expt :x 2)) (* 5 :x) 7) :x]))

  ;=> (+ (* 8 :x) 5)
```
