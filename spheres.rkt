#lang racket

(require math/bigfloat)

(define (enforce-bf arg)
  (cond
    [(bigfloat? arg) arg]
    [else (bf arg)]))

(define (ball-volume radius)
  (bf/ (bf* (bf "4")
            pi.bf
            (bfexpt (enforce-bf radius) (bf "3")))
       (bf "3")))

(define 1ae (bf "149597870700"))

(define (ae->m ae)
  (bf* (enforce-bf ae) 1ae))

(define (sphere-volume r1 r2)
  (bfabs (bf- (ball-volume (enforce-bf r1))
              (ball-volume (enforce-bf r2)))))

(define (ball-radius vol)
  (bfexpt (bf/ (bf* (enforce-bf vol)
                    (bf "3"))
               (bf* pi.bf (bf "4")))
          (bf/ (bf "1") (bf "3"))))