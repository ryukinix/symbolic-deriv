#lang racket
;;
;; Symbolic Differentiation
;; Based on lectures of Structure and Interpretation of Computer Programs.
;;

(provide deriv arg1 arg2 atom?)


(define arg1 cadr)
(define arg2 caddr)
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

(define (contains-var? exp var)
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
  (and (not (atom? exp))
       (eq? (car exp) op)))

(define (sum? exp)
  "Check if EXP it's a sum, like (+ x 2)"
  (operation? exp '+))

(define (binary-subtraction? exp)
  (and (= (length exp) 3)
       (operation? exp '-)))

(define (unary-subtraction? exp)
  (and (= (length exp) 2)
       (operation? exp '-)))

(define (product? exp)
  "Check if is a product expression."
  (operation? exp '*))

(define (power? exp var)
  "Check if EXP it's operator is a power ^ and not a exponential."
  (and (operation? exp '^)
       (contains-var? (arg1 exp) var)
       (constant? (arg2 exp) var)))

(define (exponential? exp var)
  "Check if EXP is a exponential expression of VAR."
  (and (operation? exp '^)
       (eq? (arg1 exp) 'e)
       (contains-var? (arg2 exp) var)))

(define (logarithm? exp var)
  "Check if EXP is a logarithm expression of VAR"
  #f)


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
        ((and (number? a1) (= a1 0))
         (if (number? a2) (- a2) (list '- a2)))
        ((and number? a2) (= a2 0)
         a1)
        ((same-var? a1 a2) 0)
        (else (list '- a1 a2))))

(define (make-product m1 m2)
  "Make a product expression simplified based in m1 and m2."
  (cond ((and (number? m1)
              (number? m2))
         (* m1 m2))
        ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0)))
         0)
        ((and (number? m1) (= m1 1))
         m2)

        ((and (number? m2) (= m2 1))
         m1)
        (else (list '* m1 m2))))

(define (make-power base pow)
  "Make a product expression simplified of BASE and POW."
  (cond ((and (number? pow) (= pow 1)) base)
        ((and (number? base) (= base 1)) 1)
        (else (list '^ base pow))))


(define (make-exponential base var)
  (list '^ base var))


(define (power-rule exp var)
  "Definition of the power-rule: x^n => n*x^(n-1)"
  (let* ((base (arg1 exp))
         (pow (arg2 exp))
         (new-pow (if (number? pow)
                      (- pow 1)
                      (make-subtraction pow 1))))
    (foldr make-product 1 (list (make-power base new-pow)
                                pow
                                (deriv base var)))))

(define (exponential-rule exp var)
  (let ((base (arg1 exp))
        (pow (arg2 exp)))
    (make-product (deriv pow var) (make-exponential base pow))))

(define (logarithm-rule exp var)
  (error "Not implemented yet!"))


(define (deriv exp var)
  "Main function with calculus rules for derivation."
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum (deriv (arg1 exp) var)
                   (deriv (arg2 exp) var)))
        ((binary-subtraction? exp)
         (make-subtraction (deriv (arg1 exp) var)
                           (deriv (arg2 exp) var)))
        ((unary-subtraction? exp)
         (list '- (deriv (arg1 exp) var)))
        ((product? exp)
         (make-sum (make-product (arg1 exp)
                                 (deriv (arg2 exp) var))
                   (make-product (arg2 exp)
                                 (deriv (arg1 exp) var))))
        ((power? exp var) (power-rule exp var))
        ((exponential? exp var) (exponential-rule exp var))
        ((logarithm? exp var) (logarithm-rule exp var))
        (else (error (format "Unknown rule for ~a" exp)))))
