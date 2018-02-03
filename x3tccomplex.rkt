#lang racket

(require srfi/54)

(define wares-db
  (let ((h (make-hash)))
    (for ([x '(("Energy cells" 12 16 20)
               ("Ore" 50 128 206)
               ("Silicon Wafers" 232 504 776)
               ("Tomahawk Heavy Missile" 20888 22460 24032)
               ("Flail Barrage Missile" 26954 33692 40430)
               ("Soja Husk" 204 364 524)
               ("Soja Beans" 14 28 42))])
      (hash-set! h (car x) (cdr x)))
    h))

(define (make-factory-ware name using-type num)
  (make-hash (list
              (cons 'name name)
              (cons 'using-type using-type)
              (cons 'num num))))

(define (make-factory name cycle-length factory-wares)
  (make-hash
   (list
    (cons 'name name)
    (cons 'cycle-length cycle-length)
    (cons 'wares factory-wares))))

(define (hms (h 0) (m 0) (s 0)) (+ (* h 60 60) (* m 60) s))
(define-values (1h 1m) (values (hms 1) (hms 0 1)))

(define factories-db
  (let ((h (make-hash)))
    (for ([x (list
              (list "Par Flail Missile Production Facility L" (hms 0 10) '(("Energy cells" raw -750)
                                                                           ("Ore" raw -125)
                                                                           ("Soja Husk" raw -100)
                                                                           ("Flail Barrage Missile" final 5)))
              (list "Par Tomahawk Missile Manufacturing Plant XL" (hms 0 20) '(("Energy cells" raw -3000)
                                                                               ("Ore" raw -500)
                                                                               ("Soja Husk" raw -400)
                                                                               ("Tomahawk Heavy Missile" final 30)))
              (list "Par Soyery XL" (hms 0 1) '(("Energy cells" raw -150)
                                                ("Soja Beans" raw -120)
                                                ("Soja Husk" final 20)))
              (list "Par Soyfarm XL" (hms 0 1) '(("Energy cells" raw -150)
                                                 ("Soja Beans" final 120)))
              (list "Par Ore Mine XL 1-15-30" (hms 0 1 15) '(("Energy cells" raw -180)
                                                             ("Ore" final 30)))
              (list "Par Ore Mine XL 1-3-30" (hms 0 1 3) '(("Energy cells" raw -180)
                                                           ("Ore" final 30)))
              (list "Silicon Mine XL 1-58-20" (hms 0 1 58) '(("Energy cells" raw -480)
                                                             ("Silicon Wafers" final 20))))])
      (hash-set! h (car x) (apply make-factory x)))
    h))

(define (make-complex factories-list)
  (let ((h (make-hash)))
    (for ((x factories-list))
      (if (pair? x)
          (hash-set! h (cdr x) (car x))
          (hash-set! h x 1)))
    h))

(define complex
  (make-complex (list
                 (cons 2 "Par Flail Missile Production Facility L")
                 "Par Tomahawk Missile Manufacturing Plant XL"
                 (cons 2 "Par Soyery XL")
                 (cons 2 "Par Soyfarm XL")
                 "Par Ore Mine XL 1-15-30"
                 "Par Ore Mine XL 1-3-30"
                 "Silicon Mine XL 1-58-20")))

(define (union-using-type t1 t2)
  (if (equal? t1 t2) t1 'inter))

(define (calc-complex complex (factories-db factories-db))
  (define wares-data (make-hash)) ; "name" -> (using-type . num)
 
  (define (parse-factory factory-name multiplier)
    (define factory (hash-ref factories-db factory-name))
    (define factory-wares (hash-ref factory 'wares))
    (define (calc-ware num mult)
      (/ (* num mult) (hash-ref factory 'cycle-length)))
    (for ((factory-ware factory-wares)) ; ( name using-type num)
      (match factory-ware
        [(list ware-name using-type num)
         (define wd-elem (hash-ref wares-data ware-name #f)) ; (using-type . num)
         (cond
           [wd-elem
            (hash-set! wares-data
                       ware-name
                       (cons (union-using-type (car wd-elem) using-type)
                             (+ (calc-ware num multiplier) (cdr wd-elem))))]
           [else
            (hash-set! wares-data
                       ware-name
                       (cons using-type (calc-ware num multiplier)))])])))
 
  (do ((id (hash-iterate-first complex) (hash-iterate-next complex id)))
    ((eq? id #f))
    (let ((f-num (hash-iterate-pair complex id)))
      (parse-factory (car f-num) (cdr f-num))))
  wares-data)

(define (print-balance prod-using (wares-db wares-db) (tm 1h))
  (define-values (raw inter final)
    (let loop ([wares prod-using]
               [raw '()]
               [inter '()]
               [final '()])
      (cond
        [(null? wares) (values raw inter final)]
        [else
         (define w (car wares))
         (cond
           [(equal? (cadr w) 'raw) (loop (cdr wares) (cons w raw) inter final)]
           [(equal? (cadr w) 'inter) (loop (cdr wares) raw (cons w inter) final)]
           [(equal? (cadr w) 'final) (loop (cdr wares) raw inter (cons w final))])])))
  (define (+> x y)
    (list [+ (car x) (car y)]
          [+ (cadr x) (cadr y)]
          [+ (caddr x) (caddr y)]))
  (define (+< x y)
    (list [+ (car x) (caddr y)]
          [+ (cadr x) (cadr y)]
          [+ (caddr x) (car y)]))
  (define (print-s args)
    (define-values (s1 s2 s3)
      (apply values (map (λ (x) (cat (exact->inexact x) 15 '(#\ ) 2.)) args)))
    (printf "\n~a~a~a\n" s1 s2 s3))
  (define (calc-balance items)
    (displayln '---------------------------------------------)
    (let loop ([item-list items]
               [bl '(0 0 0)])
      (cond
        [(null? item-list) bl]
        [else
         (define w (car item-list))
         (define prices (hash-ref wares-db (car w)))
         (displayln (car w))
         (define num (* (cddr w) tm))
         (displayln (cat (exact->inexact num) 15 '(#\ ) 2.))
         (if (> (cddr w) 0)
             (loop (cdr item-list) (list
                                    (+ (car bl) (* (car prices) num))
                                    (+ (cadr bl) (* (cadr prices) num))
                                    (+ (caddr bl) (* (caddr prices) num))))
             (loop (cdr item-list) (list
                                    (+ (car bl) (* (caddr prices) num))
                                    (+ (cadr bl) (* (cadr prices) num))
                                    (+ (caddr bl) (* (car prices) num)))))])))
  (define-values (sraw sinter sfinal)
    (apply values (map calc-balance (list raw inter final))))
  (displayln '---------------------RAW---------------------)
  (print-s sraw)
  (displayln '--------------------INTER--------------------)
  (print-s sinter)
  (displayln '--------------------FINAL--------------------)
  (print-s sfinal)
  (displayln '---------------------------------------------)
  (print-s (+< (+> sfinal sinter) sraw)))

(print-balance (hash->list (calc-complex complex)))
