#lang racket

(define (fac x)
  (if (< x 2)
      1
      (* x (fac (sub1 x)))))

(define (iter power x)
  (/ (expt x power) (fac power)))

(for ((n (in-range (read))))
  (define x (read))
  (displayln
   (for/sum ((power (in-range 10)))
(iter power x))))