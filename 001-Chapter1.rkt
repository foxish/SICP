#lang planet neil/sicp

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
(define (sum_sq_larger_2 x y z)
  (define x1 (max x y z))
  (define x2 (min (max x y) (max y z) (max x z)))
  (+ (square x1) (square x2)))

(sum_sq_larger_2 1 2 3)

;1.4 a-plus-abs-b
(display "a-plus-abs-b\n")
(define (a-plus-abs-b a b)
  ((if (> b 0)
       + 
       -)
   a b))

(a-plus-abs-b 10 -11)

;1.5 proof of lazy evaluation(?)
(display "Infinite Recursion\n")
(define (p) (p))
(define (func x y)
  (if (= x 0) 
      x 
      y))

;(func 0 (p))


;newton method of square roots
(display "Square Roots\n")
(define (sqrt_newton x tolerance)
  (sqrt_helper x (/ x 2.0) tolerance))

(define (sqrt_helper num guess tolerance)
  (define avg ( / (+ (/ num guess) guess) 2))
    (if (> (abs (- num (* avg avg))) tolerance)
      (sqrt_helper num avg tolerance)
      guess))

(sqrt_newton 100 0.0000001)


;newton method of cube roots
(display "Cube Roots\n")
(define (cbrt_newton x tolerance)
  (cbrt_helper x (/ x 3.0) tolerance))

(define (cbrt_helper x y tolerance)
  (define new_guess (/ (+ (/ x (* y y)) (* 2 y)) 3))
    (if (> (abs (- x (* new_guess new_guess new_guess))) tolerance)
      (cbrt_helper x new_guess tolerance)
      new_guess))

(cbrt_newton 5 0.0000001)