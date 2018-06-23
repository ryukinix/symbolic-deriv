#lang racket

(require rackunit
         rackunit/text-ui
         "deriv.rkt"
         "infix.rkt")

(define deriv-tests
  (test-suite
   "Tests for deriv.rkt"

   (check-equal? (deriv '(/ 1 x) 'x)
                 '(/ -1 (^ x 2)))
   (check-equal? (deriv '(^ x 2) 'x)
                 '(* 2 x))
   (check-equal? (deriv '(* x x) 'x)
                 '(* 2 x))
   (check-equal? (deriv '(* x t) 'x)
                 't)
   (check-equal? (deriv '(^ e (* 2 x)) 'x)
                 '(* 2 (^ e (* 2 x))))))

(define infix-tests
  (test-suite
   "Tests for infix.rkt"

   (check-equal? (deriv->infix '(1 / x) 'x)
                 '(-1 / (x ^ 2)))
   (check-equal? (deriv->infix '(x ^ 2) 'x)
                 '(2 * x))
   (check-equal? (deriv->infix '(x * x) 'x)
                 '(2 * x))
   (check-equal? (deriv->infix '(x * t) 'x)
                 't)
   (check-equal? (deriv->infix '(e ^ (2 * x)) 'x)
                 '(2 * (e ^ (2 * x))))))


(display "Running tests for deriv.rkt...\n")
(run-tests deriv-tests)
(display "Running tests for infix.rkt...\n")
(run-tests infix-tests)
