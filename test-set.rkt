#lang racket

(define (test-set a b c)
  (define result 0)
  (for ((x (in-range a b c)))
    (set! result (+ result x)))
  result)

(define (test-box a b c)
  (define result (box 0))
  (for ((x (in-range a b c)))
    (set-box! result (+ (unbox result) x)))
  (unbox result))

(define (test-set-do a b c)
  (define result 0)
  (do ((x a (+ x c)))
    ((>= x b) result)
    (set! result (+ result x))))

(define (test-box-do a b c)
  (define result (box 0))
  (do ((x a (+ x c)))
    ((>= x b) (unbox result))
    (set-box! result (+ (unbox result) x))))

(define (test-rec a b c)
  (letrec
      ((fun
        (λ (x)
          (if (>= x b)
              0
              (+ x (fun (+ x c)))))))
    (fun a)))

(define (test-rec-acc a b c)
  (letrec
      ((fun
        (λ (x summ)
          (if (>= x b)
              summ
              (fun (+ x c) (+ summ x))))))
    (fun a 0)))

(define (test-rec-acc-rev a b c)
  (letrec
      ((fun
        (λ (x summ)
          (if (< x b)
              (fun (+ x c) (+ summ x))
              summ))))
    (fun a 0)))
