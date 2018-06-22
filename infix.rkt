#lang racket

(require "deriv.rkt")

(define operators-precedence '(^ * / + -))

(define (prefix->infix exp)
  "Translate prefix notation to infix notation: (* 2 3) => (2 * 3)"
  (cond ((null? exp) '())
        ((atom? exp) exp)
        (else (list (prefix->infix (arg1 exp))
                    (car exp)
                    (prefix->infix (arg2 exp))))))

(define (infix->prefix exp)
  "Parse infix expression to infix: (3 + (2 ^ 3)) => (+ 3 (^ 2 3)) "
  (error "To be implemented."))

(define (deriv->infix exp var)
  "Derivation using infix notation as output"
  (prefix->infix (deriv exp var)))
