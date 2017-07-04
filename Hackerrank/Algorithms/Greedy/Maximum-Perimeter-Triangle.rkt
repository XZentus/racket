#lang racket

(define (check-p-unsafe lst)
  (define-values
    (a b c) (values (car lst) (cadr lst) (caddr lst)))
  (if (< a (+ b c))
      (+ a b c)
      -1))

(define (routine arg)
  (letrec
      ((fun
        (λ (lst)
          (cond
            ((null? (cddr lst)) -1)
            (else
             (if (< 0 (check-p-unsafe lst))
                 (list (caddr lst) (cadr lst) (car lst))
                 (fun (cdr lst))))))))
    (fun arg)))

(define (main)
  (define sticks
    (sort
     (for/list ((_ (in-range (read)))) (read))
     >))
  (define result (routine sticks))
  (apply printf
         (if (list? result)
             (cons "~a ~a ~a~%" result)
             (list "~a~%" result))))

(main)
