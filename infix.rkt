#lang racket

(require "deriv.rkt")

(define operators-precedence '(^ * / + -))

(define (prefix->infix exp)
  (cond ((null? exp) '())
        ((atom? exp) exp)
        (else (list (prefix->infix (arg1 exp))
                    (car exp)
                    (prefix->infix (arg2 exp))))))

(define (infix->prefix exp)
  (error "To be implemented."))

(define (deriv->infix exp var)
  (prefix->infix (deriv exp var)))
