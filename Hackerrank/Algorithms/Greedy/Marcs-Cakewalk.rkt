#lang racket

(define (gen-miles calories)
  (for/sum ((i (in-naturals))
            (c calories))
    (* c (expt 2 i))))

(define (main)
  (displayln
   (gen-miles (sort (for/list ((_ (in-range (read)))) (read)) >))))

(main)
