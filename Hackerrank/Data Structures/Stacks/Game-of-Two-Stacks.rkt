#lang racket

(define (get-data vec pos limit)
  (letrec
      ((routine
        (λ (pos sum)
          (if (>= pos (vector-length vec))
              (values pos sum)
              (let ((next-sum (+ sum (vector-ref vec pos))))
                (if (> next-sum limit)
                    (values pos sum)
                    (routine (add1 pos) next-sum)))))))
    (routine pos 0)))

(define (check-len stack1 stack2 limit)
  (letrec
      ((routine
        (λ (max-n pos1 sum1 pos2 sum2)
          (cond
            ((< pos1 0) max-n)
            (else
             (define-values (pos2-n sum2-n) (get-data stack2 pos2 (- limit sum1 sum2)))
             (define next-num (+ pos1 pos2-n))
             (define next-max (max max-n next-num))
             (if (or (< pos1 1)
                     (= pos2 (sub1 (vector-length stack2))))
                 next-max
                 (routine next-max (sub1 pos1) (- sum1 (vector-ref stack1 (sub1 pos1))) pos2-n (+ sum2 sum2-n))))))))
    (let*-values (((start-pos-1 start-sum-1) (get-data stack1 0 limit))
                  ((start-pos-2 start-sum-2) (get-data stack2 0 (- limit start-sum-1))))
      (routine (+ start-pos-1 start-pos-2) start-pos-1 start-sum-1 start-pos-2 start-sum-2))))
  

(define (test)
  (define-values (stack1-length stack2-length limit)
    (values (read) (read) (read)))
  (define stack1 (for/vector ((_ (in-range stack1-length))) (read)))
  (define stack2 (for/vector ((_ (in-range stack2-length))) (read)))
  (displayln (check-len stack1 stack2 limit)))



(define (main)
  (for ((_ (in-range (read)))) (test)))
(main)
