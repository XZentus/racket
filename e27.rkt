#lang racket

(define *primes* (reverse '[2 3 5 7 11 13 17 19 23]))

(define (get-next-primes limit)
  (define (next-prime)
    (let outer ((ps *primes*)
                (n (+ 2 (car *primes*))))
      (cond
        ((null? ps)
         (set! *primes*
               (cons n *primes*)))
        ((= 0 (remainder n (car ps)))
         (outer *primes* (+ 2 n)))
        (else (outer (cdr ps) n)))))
  (do ()
    ((>= (car *primes*) limit))
    (next-prime)))

(define (primes-seq-len a b)
  (let loop ((n 0)
             (p b))
    (when (> p (car *primes*))
      (get-next-primes p))
    (let ((n1 (add1 n)))
      (if (member p *primes*)
          (loop n1
                (+ (* n1 n1)
                   (* n1 a)
                   b))
          n))))

(define (main)
  (define result '(0 0 0))
  (for ((a (range -999 1000)))
    (for ((b (range -1000 1001)))
      (define tmp (primes-seq-len a b))
      (when (> tmp (caddr result))
        (set! result (list a b tmp)))))
  (printf "~A ~A~%" result (* (car result) (cadr result))))

(main)

  ;main :: IO ()
  ;main = do
  ;    let aRange = [-999 .. 999]
  ;        bRange = [-1000 .. 1000]
  ;        raw = flip runState primesInit $ sequence $ [getPrimeSeqLen a b | a <- aRange, b <- bRange]
  ;        primesSeqList = fst raw
  ;--        test = map (uncurry3 QPL) [(1, 2, 3), (3, 4, 5), (10, 20, 0), (-100, 100, 100)] where
  ;--          uncurry3 f (a, b, c) = f a b c
  ;        result = foldl' max (head primesSeqList) (tail primesSeqList)
  ;--    print $ foldl' max (head test) (tail test)
  ;    print $ snd $ raw
  ;    print result
  ;print $ a result * b result