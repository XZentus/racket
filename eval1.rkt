#lang racket

(require plot)

(define function-probability 0.8)
(define arg-probability 0.7)
(define min-val -2)
(define max-val  2)
(define mutate-arg 0.1)
(define mutate-num 0.6)
(define mutate-fun 0.2)
(define (mutate-min x) -1)
(define (mutate-max x)  1)

(define fitness-min -7)
(define fitness-max  7)
(define fitness-points 100)

(define population-size 50)
(define individuals-survive 20)
(define exception-weight 1000)

(define gens
  #((+ expr expr)
    (- expr expr)
    (* expr expr)
    (/ expr expr)
    ;(expt expr expr)
    (sin expr)
    (cos expr)
    (tan expr)))

(define args
  #(x))

(define args-list
  (vector->list args))

(define gens-number (vector-length gens))
(define args-number (vector-length args))

(define (target-fun x)
  (+ (* x 1.46327) (/ x 0.3423)))

(define (fitness-compare f1 f2)
  (for/sum ([x (in-range fitness-min
                         fitness-max
                         (exact->inexact (/ (- fitness-max fitness-min) fitness-points)))])
    (abs (- (f1 x) (f2 x)))))

(define (generate-values fun)
  (for/list ([x (in-range fitness-min
                          fitness-max
                          (exact->inexact (/ (- fitness-max fitness-min) fitness-points)))])
    (with-handlers
        ([exn:fail? (lambda (exn) exception-weight)])
      (fun x))))

(define (random-real-interval from to)
  (+ from (* (random) (- to from))))

(define (gen-fun depth)
  (define prob (random))
  {if [or (< depth 1) (> prob function-probability)]
      [if (< (random) arg-probability)
          (vector-ref args (random args-number))
          (random-real-interval min-val max-val)]
      [let ([expr (vector-ref gens (random gens-number))])
        (if (list? expr)
            (map (lambda (x)
                   (if (eq? x 'expr) (gen-fun (sub1 depth))
                       x))
                 expr)
            expr)]})

(define (simplify-routine arg)
  (cond
    ((or (symbol? arg) (number? arg)) arg)
    ((list? arg)
     (define fun (car arg))
     (define arg1 (simplify-routine (cadr arg)))
     (define arg2 (if (null? (cddr arg))
                      '()
                      (simplify-routine (caddr arg))))
     (cond
       ((null? arg2)
        (if (number? arg1)
            (eval `(,fun arg1))
            (list fun arg1)))
       ((eq? fun '+)
        (cond
          ((equal? arg1 arg2)
           (simplify-routine `(* 2 ,arg1)))
          ((and (number? arg1) (number? arg2))
           (+ arg1 arg2))
          ((and (number? arg1) (= arg1 0))
           arg2)
          ((and (number? arg2) (= arg2 0))
           arg1)
          (else
           `(+ ,arg1 ,arg2))))
       ((eq? fun '-)
        (cond
          ((equal? arg1 arg2)                    0)
          ((and (number? arg1) (number? arg2))  (- arg1 arg2))
          ((and (number? arg2) (= arg2 0))      arg1)
          ((and (number? arg1) (= arg1 0) (number? arg2)) (- arg2))
          (else                                   `(- ,arg1 ,arg2))))
       ((eq? fun '*)
        (cond
          ((and (number? arg1) (number? arg2)) (* arg1 arg2))
          ((and (number? arg1) (= arg1 1))     arg2)
          ((and (number? arg2) (= arg2 1))     arg1)
          ((and (number? arg1) (= arg1 -1) (number? arg2)) (- arg2))
          ((and (number? arg2) (= arg2 -1) (number? arg1)) (- arg1))
          (else                                  `(* ,arg1 ,arg2))))
       ((eq? fun '/)
        (cond
          ((or (and (number? arg1) (= arg1 0))
               (and (number? arg2) (= arg2 0))) (error "division by zero"))
          ((equal? arg1 arg2)   1)
          ((and (number? arg2) (= arg2 1))     arg1)
          ((and (number? arg1) (number? arg2)) (/ arg1 arg2))
          (else                               `(/ ,arg1 ,arg2))))
       (else arg)))
    (else arg)))

(define (simplify arg)
  (with-handlers
      [(exn:fail? (lambda (exn) exception-weight))]
    (simplify-routine arg)))

(define (mutate fun depth)
  (define mut (random))
  {cond
    [(and (symbol? fun) (< mut mutate-arg))
     (gen-fun depth)]
    [(and (number? fun) (< mut mutate-num))
     (+ fun (random-real-interval (mutate-min fun) (mutate-max fun)))]
    [(and (list? fun) (< mut mutate-fun))
     (gen-fun (sub1 depth))]
    [(list? fun)
     (cons (car fun) (map (λ (x) (mutate x (sub1 depth))) (cdr fun)))]
    [else
     fun]})
 
(define (test1 d x)
  (define var (gen-fun d))
  (printf "(~A)(~A) = ~A~%" var x (eval `((λ (x) ,var) ,x))))

(define (compare-data data1 data2)
  (for/sum [(d1 data1) (d2 data2)]
    (abs (- (real-part d1) (real-part d2)))))

(define (evaluate d iterations fun)
  (define values (generate-values fun))
  (define population
    (for/vector ((x (in-range population-size)))
      (define gfun (gen-fun d))
      (define gfun-data (generate-values (eval `(λ ,args-list ,(simplify gfun)))))
      (cons gfun (compare-data values gfun-data))))
  {for ((n (in-range iterations)))
    [vector-sort! population < #:key cdr]
    [for ((i (in-range individuals-survive)))     
      (define mut-fun (mutate (car (vector-ref population i)) d))
      (define mut-vals (generate-values (eval `(λ ,args-list ,(simplify mut-fun)))))
      (define result (compare-data values mut-vals))
      (when (< result (cdr (vector-ref population i)))
        (vector-set! population i (cons mut-fun result)))]
    [for ((i (in-range individuals-survive population-size)))
      (define gfun (gen-fun d))
      (define gfun-data (generate-values (eval `(λ ,args-list ,(simplify gfun)))))
      (vector-set! population i (cons gfun (compare-data values gfun-data)))]
    (when (= 0 (remainder n 100)) (printf "Iteration ~A...~%" n))}
  population)

(define (enforce-compile arg)
  (if (procedure? arg)
      arg
      (eval `(λ ,args-list ,arg))))

(define (plot2 f1 f2)
  (plot (list (function (enforce-compile f1) min-val max-val)
              (function (enforce-compile f2) min-val max-val))))