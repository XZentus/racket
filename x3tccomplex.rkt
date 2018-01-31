#lang racket

(define (make-ware name min-price med-price max-price)
  (make-hash (list
              (cons 'name name)
              (cons 'min-price min-price)
              (cons 'med-price med-price)
              (cons 'max-price max-price))))

(define wares-db
  (let ((h (make-hash)))
    (for ([x '(("Батареи"                              12    16    20)
               ("Руда"                                 50   128   206)
               ("Кремниевые пластины"                 232   504   776)
               ("Тяжелая ракета \"Томагавк\""       20888 22460 24032)
               ("Заградительная ракета \"Булава\""  26954 33692 40430)
               ("Соевая мука"                         204   364   524)
               ("Соевые бобы"                          14    28    42))])
      (hash-set! h (car x) (apply make-ware x)))
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
              (list "Пар Завод ракет \"Булава\" L" (hms 0 10) '(("Батареи" 'raw -750)
                                                                ("Руда" 'raw -125)
                                                                ("Соевая мука" 'raw -100)
                                                                ("Заградительная ракета \"Булава\"" 'final 5)))
              (list "Пар Завод ракет \"Томагавк\" XL" (hms 0 20) '(("Батареи" 'raw -3000)
                                                                   ("Руда" 'raw -500)
                                                                   ("Соевая мука" 'raw -400)
                                                                   ("Тяжелая ракета \"Томагавк\"" 'final 30)))
              (list "Пар Соевая фабрика XL" (hms 0 1) '(("Батареи" 'raw -150)
                                                        ("Соевые бобы" 'raw -120)
                                                        ("Соевая мука" 'final 20)))
              (list "Пар Соевая ферма XL" (hms 0 1) '(("Батареи" 'raw -150)
                                                      ("Соевые бобы" 'final 120)))
              (list "Рудная шахта XL 1-15-30" (hms 0 1 15) '(("Батареи" 'raw -180)
                                                             ("Руда" 'final 30)))
              (list "Рудная шахта XL 1-3-30" (hms 0 1 15) '(("Батареи" 'raw -180)
                                                            ("Руда" 'final 30)))
              (list "Кремниевая шахта XL 1-58-20" (hms 0 1 58) '(("Батареи" 'raw -480)
                                                                 ("Кремниевые пластины" 'final 20))))])
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
                 (cons 2 "Пар Завод ракет \"Булава\" L")
                 "Пар Завод ракет \"Томагавк\" XL"
                 (cons 2 "Пар Соевая фабрика XL")
                 (cons 2 "Пар Соевая ферма XL")
                 "Рудная шахта XL 1-15-30"
                 "Рудная шахта XL 1-3-30"
                 "Кремниевая шахта XL 1-58-20")))

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
        
