#lang racket
;;
;; Symbolic Differentiation
;; Based on lectures of Structure and Interpretation of Computer Programs.
;;

(provide deriv arg1 arg2 atom?)


#|

Implemented rules:

+ Subtraction
+ Product
+ Quotient/Division
+ Sum
+ Power
+ Exponential of e

|#

(define arg1 cadr)
(define arg2 caddr)
(define ^ expt) ;; power symbol function

#|

== EXPRESSION ANALYSIS ==

|#

;; Check if a EXP it's a simple atom: that means is not a pair-cons cell.
(define (atom? exp)
  (not (pair? exp)))

;; Check if EXP is a constant based in VAR argument.
(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

;; Check if EXP and VAR it's the same variable.
(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

;; Check if the EXP tree has at least one VAR symbol.
(define (contains-var? exp var)
  (cond ((same-var? exp var) #t)
        ((atom? exp) #f)
        ((or (and (>= (length exp) 2)
                  (contains-var? (arg1 exp) var))
             (and (>= (length exp) 3)
                  (contains-var? (arg2 exp) var))) #t)
        (else #f)))

;; Check if a given EXP is a operation of symbol OP.
;; Ex.: (operation? '(* x x) '*) => #t
(define (operation? exp op)
  (and (pair? exp)
       (eq? (car exp) op)))

;; Use operation? procedure with a constraint of exp being a binary operation
(define (binary-op? exp op)
  (and (operation? exp op)
       (= (length exp) 3)))

;; Use operation? procedure with a constraint of exp being a unary operation
(define (unary-op? exp op)
  (and (operation? exp op)
       (= (length exp) 2)))

;; Check if EXP it's a sum, like (+ x 2)
(define (sum? exp)
  (binary-op? exp '+))

;; Check if the EXP is a binary exp as (- x y)
(define (binary-subtraction? exp)
  (binary-op? exp '-))

;; Check if the EXP is a unary exp as (- x)
(define (unary-subtraction? exp)
  (unary-op? exp '-))

;; Check if is a product expression.
(define (product? exp)
  (binary-op? exp '*))

(define (division? exp)
  (binary-op? exp '/))

;; Check if EXP it's operator is a power ^ and not a exponential.
(define (power? exp var)
  (and (binary-op? exp '^)
       (contains-var? (arg1 exp) var)
       (not (contains-var? (arg2 exp) var))))

;; Check if EXP is a exponential expression of VAR.
(define (exponential? exp var)
  (and (binary-op? exp '^)
       (eq? (arg1 exp) 'e)
       (contains-var? (arg2 exp) var)))

#|

== SYMBOLIC EXPRESSIONS CONSTRUCTIONS ==

|#

;; Make a sum expression based in a1 and a2 already simplified.
(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        ((same-var? a1 a2) (list '* 2 a1))
        (else (list '+ a1 a2))))

;; Make a subtraction expression simplified
(define (make-subtraction a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (- a1 a2))
        ((and (number? a1)
              (= a1 0))
         (if (number? a2)
             (- a2)
             (list '- a2)))
        ((and (number? a2)
              (= a2 0))
         a1)
        ((same-var? a1 a2) 0)
        (else (list '- a1 a2))))

;; Make a product expression simplified based in m1 and m2.
(define (make-product m1 m2)
  (cond ((and (number? m1)
              (number? m2)) (* m1 m2))
        ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0))) 0)
        ((and (number? m1) (= m1 1)) m2)

        ((and (number? m2) (= m2 1)) m1)
        (else (list '* m1 m2))))

;; Make a division expression based in a and b.
(define (make-division a b)
  (cond ((and (number? a)
              (number? b)) (/ a b))
        ((and (number? a) (= a 0)) 0)
        ((and (number? b) (= b 1)) a)
        (else (list '/ a b))))

;; Make a product expression simplified of BASE and POW.
(define (make-power base pow)
  (cond ((and (number? pow) (= pow 1)) base)
        ((and (number? pow) (= pow 0)) 1)
        ((and (number? base) (= base 1)) 1)
        (else (list '^ base pow))))

#|

== CALCULUS DERIVATION RULES ==

|#

;; Definition of the sum rule: f(x) + g(x) => f'(x) + g'(x)
(define (sum-rule exp var)
  (make-sum (deriv (arg1 exp) var)
            (deriv (arg2 exp) var)))

;; Definition of the power rule: x^n => n*x^(n-1)
(define (power-rule exp var)
  (let* ((base (arg1 exp))
         (pow (arg2 exp))
         (new-pow (if (number? pow)
                      (- pow 1)
                      (make-subtraction pow 1))))
    (make-product (make-product pow (deriv base var))
                  (make-power base new-pow))))

;; Definition of the product rule: f(x)g(x) => f'(x)g(x) + f(x)g'(x)
(define (product-rule exp var)
  (make-sum (make-product (arg1 exp)
                          (deriv (arg2 exp) var))
            (make-product (arg2 exp)
                          (deriv (arg1 exp) var))))

;; Definition of the division rule: f(x)/g(x) => (f'(x)g(x)-f(x)g'(x))/(g(x))²
(define (division-rule exp var)
  (let ((f (arg1 exp))
        (g (arg2 exp)))
    (make-division (make-subtraction (make-product (deriv f var) g)
                                     (make-product f (deriv g var)))
                   (make-power g 2))))

;; Use of the chain-rule for the exponential: e^p(x) => p'(x)*e^p(x)
(define (exponential-rule exp var)
  (let ((base (arg1 exp))
        (pow (arg2 exp)))
    (make-product (deriv pow var) (make-power base pow))))

;; MAIN ALGORITHM

;; Main function with calculus rules for derivation.
(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp) (sum-rule exp var))
        ((binary-subtraction? exp) (make-subtraction (deriv (arg1 exp) var)
                                                     (deriv (arg2 exp) var)))
        ((unary-subtraction? exp) (list '- (deriv (arg1 exp) var)))
        ((product? exp) (product-rule exp var))
        ((division? exp) (division-rule exp var))
        ((power? exp var) (power-rule exp var))
        ((exponential? exp var) (exponential-rule exp var))
        (else (error (format "Unknown derivation rule for exp: ~a." exp)))))
