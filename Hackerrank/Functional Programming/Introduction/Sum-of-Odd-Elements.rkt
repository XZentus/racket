#lang racket

(define (read-input-to-list)
  (define param (read))
  (if (eq? param eof)
      '()
      (cons param (read-input-to-list))))

(displayln
 (for/sum ((x (read-input-to-list)))
   (if (odd? x)
       x
       0)))
