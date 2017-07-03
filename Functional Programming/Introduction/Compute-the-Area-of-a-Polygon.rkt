#lang racket

(define (vec-2d-diff v1 v2)
  (let ([x1 (car v1)] [y1 (cdr v1)]
        [x2 (car v2)] [y2 (cdr v2)])
    (cons (- x1 x2) (- y1 y2))))

(define (cross-prod-2d v1 v2)
  (let ([x1 (car v1)] [y1 (cdr v1)]
        [x2 (car v2)] [y2 (cdr v2)])
    (- (* x1 y2) (* x2 y1))))

(define (polyarea lst)
  (let ([pivot (first lst)])
    (/
     (for/sum ([v1 lst] [v2 (rest lst)])
              (cross-prod-2d (vec-2d-diff v1 pivot) (vec-2d-diff v2 v1)))
     2)))

(define polygon (for/list ([_ (read)]) (cons (read) (read))))
(displayln (exact->inexact (polyarea polygon)))
