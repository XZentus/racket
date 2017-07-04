#lang racket

(define (read-input-to-list)
  (define param (read))
  (if (eq? param eof)
      '()
      (cons param (read-input-to-list))))

(letrec
    ((odd-filter
      (λ (lst is-even?)
        (cond
          ((pair? lst)
           (when is-even? (displayln (first lst)))
           (odd-filter (rest lst) (not is-even?)))))))
  (odd-filter (read-input-to-list) #f))
