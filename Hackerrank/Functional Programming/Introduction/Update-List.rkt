#lang racket

(define (f lst)
  (if (pair? lst)
      (cons
       (let ((x (first lst)))
         (if (number? x)
             (abs x)
             x))
       (f (rest lst)))
      null))

(define (read-list)
  (let ([x (read)]) 
    (if (eof-object? x)
        (list)
        (cons x (read-list)))))

(let ([lst (read-list)]) 
  (let ([ans (f lst)])
    (for ([x ans])
(printf "~a\n" x))))
