#lang racket

(define s (read))

(define (read-input-to-list)
  (define param (read))
  (if (eq? param eof)
      '()
      (cons param (read-input-to-list))))

(for ((x (read-input-to-list)))
  (for ((i (in-range s)))
    (displayln x)))
