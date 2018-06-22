#lang racket
;;
;; Symbolic Differentiation
;; Based on lectures of Structure and Interpretation of Computer Programs.
;;

(define op car)
(define a1 cadr)
(define a2 caddr)
(define m1 cadr)
(define m2 caddr)
(define ^ expt) ;; power symbol function


(define (atom? exp)
  "Check if a EXP it's a simple atom: that means is not a pair-cons cell."
  (not (pair? exp)))

(define (constant? exp var)
  "Check if EXP is a constant based in VAR argument."
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  "Check if EXP and VAR it's the same variable."
  (and (atom? exp)
       (eq? exp var)))

(define (operation? exp op)
  "Check if a given EXP is a operation of symbol OP.
   Ex.: (operation? '(* x x) '*) => #t"
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (sum? exp)
  "Check if EXP it's a sum, like (+ x 2)"
  (operation? exp '+))

(define (power? exp)
  "Check if EXP it's operator is a power ^."
  (operation? exp '^))

(define (product? exp)
  "Check if is a product expression."
  (operation? exp '*))

;; First definitions of make-sum and make-product without simplifications

;; (define make-sum (a1 a2)
;;   (list '+ a1 a2))

;; (define make-product (m1 m2)
;;   (list '* m1 m2))

;; second version of representation
;; simplifying algebraic expressions

(define (make-sum a1 a2)
  "Make a sum expression based in a1 and a2 already simplified."
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        ((same-var? a1 a2) (list '* 2 a1))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  "Make a product expression simplified based in a1 and a2."
  (cond ((and (number? m1)
              (number? m2))
         (+ m1 m2))
        ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0)))
         0)
        ((and (number? m1) (= m1 1))
         m2)

        ((and (number? m2) (= m2 1))
         m1)
        (else (list '* m1 m2))))

;; FIXME: chain rule need be used
(define (power-rule exp)
  "Definition of the power-rule: x^n => n*x^(n-1)"
  (let ((base (cadr exp))
        (pow (caddr exp)))
    (list '* pow (list '^ base (- pow 1)))))

(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var)))
        ((product? exp)
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (m2 exp)
                        (deriv (m1 exp) var))))
        ((power? exp) (power-rule exp))
        (else (error "Unknown expression."))))

;; (deriv (* x x) 'x)
