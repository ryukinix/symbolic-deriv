#lang racket

(require "deriv.rkt")

(define operators-precedence '(^ * / + -))

;; Translate prefix notation to infix notation: (* 2 3) => (2 * 3).
(define (prefix->infix exp)
  (cond ((null? exp) '())
        ((atom? exp) exp)
        (else (list (prefix->infix (arg1 exp))
                    (car exp)
                    (prefix->infix (arg2 exp))))))

;; FIXME: this algorithm is just a Left-to-Right parser without precedence
;; Parse infix expression to infix: (3 + (2 ^ 3)) => (+ 3 (^ 2 3)).
(define (infix->prefix exp)
  (cond ((atom? exp) exp)
        ((null? (cdr exp)) (infix->prefix (car exp)))
        (else (list (cadr exp)
                    (infix->prefix (car exp))
                    (infix->prefix (cddr exp))))))

;; Derivation using infix notation as input and output.
(define (deriv->infix exp var)
  (prefix->infix (deriv (infix->prefix exp) var)))
