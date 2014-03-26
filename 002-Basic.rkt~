#lang racket

;squares
(define (square size) (* size size))
(define (abs x)
  (if (< x 0) 
      (- x) 
      x))
(abs (- (square 10)))

;comparison
(define a 1)
(define b 1)
(= a b)


;1.3 Define a procedure which takes 3 numbers as arguments and returns the sum of squares of two larger numbers
(format "procedure")
(define (sum_sq_larger_2 x y z)
  (define x1 (max x y z))
  (define x2 (min (max x y) (max y z) (max x z)))
  (+ (square x1) (square x2)))

(sum_sq_larger_2 1 2 3)

;1.4 a-plus-abs-b
(format "a-plus-abs-b")
(define (a-plus-abs-b a b)
  ((if (> b 0)
       + 
       -)
   a b))

(a-plus-abs-b 10 -11)

;1.5 proof of lazy evaluation(?)
(format "Infinite Recursion")
(define (p) (p))
(define (func x y)
  (if (= x 0) 
      x 
      y))

;(func 0 (p))


;newton method of square roots
(format "Square Roots")
(define (sqrt_newton x tolerance)
  (sqrt_helper x (/ x 2.0) tolerance))

(define (sqrt_helper num guess tolerance)
  (define avg ( / (+ (/ num guess) guess) 2))
    (if (> (abs (- num (* avg avg))) tolerance)
      (sqrt_helper num avg tolerance)
      guess))

(sqrt_newton 100 0.0000001)
