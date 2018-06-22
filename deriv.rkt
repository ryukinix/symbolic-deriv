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

== CHECKERS ==

|#

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

(define (contains-var? exp var)
  "Check if the EXP tree has at least one VAR symbol."
  (cond ((null? exp) #f)
        ((number? exp) #f)
        ((same-var? exp var) #t)
        ((atom? exp) #f)
        ((same-var? (arg1 exp) var) #t)
        ((< (length exp) 3) #f)
        ((same-var? (arg2 exp) var) #t)
        ((or (contains-var? (arg1 exp) var)
             (contains-var? (arg2 exp) var)) #t)
        (else #f)))

(define (operation? exp op)
  "Check if a given EXP is a operation of symbol OP.
   Ex.: (operation? '(* x x) '*) => #t"
  (and (pair? exp)
       (eq? (car exp) op)))

(define (binary-op? exp op)
  "Use operation? procedure with a constraint of exp being a binary operation"
  (and (operation? exp op)
       (= (length exp) 3)))

(define (unary-op? exp op)
  "Use operation? procedure with a constraint of exp being a unary operation"
  (and (operation? exp op)
       (= (length exp) 2)))

(define (sum? exp)
  "Check if EXP it's a sum, like (+ x 2)"
  (binary-op? exp '+))

(define (binary-subtraction? exp)
  "Check if the EXP is a binary exp as (- x y)"
  (binary-op? exp '-))

(define (unary-subtraction? exp)
  "Check if the EXP is a unary exp as (- x)"
  (unary-op? exp '-))

(define (product? exp)
  "Check if is a product expression."
  (binary-op? exp '*))

(define (division? exp)
  (binary-op? exp '/))

(define (power? exp var)
  "Check if EXP it's operator is a power ^ and not a exponential."
  (and (binary-op? exp '^)
       (contains-var? (arg1 exp) var)
       (or (constant? (arg2 exp) var)
           (not (contains-var? (arg2 exp) var)))))

(define (exponential? exp var)
  "Check if EXP is a exponential expression of VAR."
  (and (binary-op? exp '^)
       (eq? (arg1 exp) 'e)
       (contains-var? (arg2 exp) var)))

#|

== SYMBOLIC EXPRESSIONS CONSTRUCTIONS ==

|#

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

(define (make-subtraction a1 a2)
  "Make a subtraction expression simplified"
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

(define (make-product m1 m2)
  "Make a product expression simplified based in m1 and m2."
  (cond ((and (number? m1)
              (number? m2)) (* m1 m2))
        ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0))) 0)
        ((and (number? m1) (= m1 1)) m2)

        ((and (number? m2) (= m2 1)) m1)
        (else (list '* m1 m2))))

(define (make-division a b)
  "Make a division expression based in a and b."
  (cond ((and (number? a)
              (number? b)) (/ a b))
        ((and (number? a) (= a 0)) 0)
        ((and (number? b) (= b 1)) a)
        (else (list '/ a b))))

(define (make-power base pow)
  "Make a product expression simplified of BASE and POW."
  (cond ((and (number? pow) (= pow 1)) base)
        ((and (number? base) (= base 1)) 1)
        (else (list '^ base pow))))

#|

== CALCULUS DERIVATION RULES ==

|#

(define (sum-rule exp var)
  "Definition of the sum rule: f(x) + g(x) => f'(x) + g'(x)"
  (make-sum (deriv (arg1 exp) var)
            (deriv (arg2 exp) var)))

(define (power-rule exp var)
  "Definition of the power rule: x^n => n*x^(n-1)"
  (let* ((base (arg1 exp))
         (pow (arg2 exp))
         (new-pow (if (number? pow)
                      (- pow 1)
                      (make-subtraction pow 1))))
    (foldr make-product 1 (list (make-power base new-pow)
                                pow
                                (deriv base var)))))

(define (product-rule exp var)
  "Definition of the product rule: f(x)g(x) => f'(x)g(x) + f(x)g'(x)"
  (make-sum (make-product (arg1 exp)
                          (deriv (arg2 exp) var))
            (make-product (arg2 exp)
                          (deriv (arg1 exp) var))))

(define (division-rule exp var)
  "Definition of the division rule: f(x)/g(x) => (f'(x)g(x)-f(x)g'(x))/(g(x))²"
  (let ((f (arg1 exp))
        (g (arg2 exp)))
    (make-division (make-subtraction (make-product (deriv f var) g)
                                     (make-product f (deriv g var)))
                   (make-power g 2))))

(define (exponential-rule exp var)
  "Use of the chain-rule for the exponential: e^p(x) => p'(x)*e^p(x)"
  (let ((base (arg1 exp))
        (pow (arg2 exp)))
    (make-product (deriv pow var) (make-power base pow))))

;; MAIN ALGORITHM

(define (deriv exp var)
  "Main function with calculus rules for derivation."
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
