#lang racket

(define (read-input-to-list)
  (define param (read))
  (if (eq? param eof)
      '()
      (cons param (read-input-to-list))))

(define (print-reverse lst)
  (when (pair? lst)
    (print-reverse (rest lst))
    (displayln (first lst))))

(print-reverse (read-input-to-list))
