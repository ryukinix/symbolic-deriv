#lang racket

(require "deriv.rkt")

(define operators-precedence '(^ * / + -))

(define (prefix->infix exp)
  "Translate prefix notation to infix notation: (* 2 3) => (2 * 3)."
  (cond ((null? exp) '())
        ((atom? exp) exp)
        (else (list (prefix->infix (arg1 exp))
                    (car exp)
                    (prefix->infix (arg2 exp))))))

;; FIXME: this algorithm is just a Left-to-Right parser without precedence
(define (infix->prefix exp)
  "Parse infix expression to infix: (3 + (2 ^ 3)) => (+ 3 (^ 2 3))."
  (cond ((atom? exp) exp)
        ((null? (cdr exp)) (infix->prefix (car exp)))
        (else (list (cadr exp)
                    (infix->prefix (car exp))
                    (infix->prefix (cddr exp))))))

(define (deriv->infix exp var)
  "Derivation using infix notation as input and output."
  (prefix->infix (deriv (infix->prefix exp) var)))
