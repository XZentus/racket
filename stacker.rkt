#lang racket

(require compatibility/defmacro)

(define (datum? x)
  (or (list? x) (symbol? x)))

(define (blank? str)
  (for/and ([c (in-string str)])
    (char-blank? c)))

(define (string->datum str)
  (unless (blank? str)
    (let ([result (read (open-input-string (format "(~A)" str)))])
      (if (= (length result) 1)
          (first result)
          result))))

(define (format-datum template . args)
  (unless (datum? template)
    (raise-argument-error 'format-datum "datum?" template))
  (string->datum
   (apply format
          (format "~A" template)
          (map (λ (arg) (if (syntax? arg)
                            (syntax->datum arg)
                            arg))
               args))))

(define (format-datums template . argss)
  (unless (datum? template)
    (raise-argument-error 'format-datums "datum?" template))
  (apply map (λ args (apply format-datum template args)) argss))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt" ,@src-datums))
  (datum->syntax #f module-datum))

(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin HANDLE-EXPR ...))

(provide (rename-out [stacker-module-begin #%module-begin]))