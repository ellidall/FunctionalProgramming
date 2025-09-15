#lang racket
(require racket/generator)

;1
(define (check-decimal-digits lst)
  (for/list ([value lst]
             [id (in-naturals 1)]
             #:unless (and (integer? value) (<= 0 value 9)))
    (cons id value)))
(check-decimal-digits '(1 4 2 7 32 9 88 2 0 one))

;2
(define (fib-like? lst)
  (if (< (length lst) 3)
      #f
      (for/and ([pre-prev lst]
                [prev (rest lst)]
                [cur (rest (rest lst))])
        (= cur (+ pre-prev prev)))))

(fib-like? '(0 1 1 2 3 5 8 13))
(fib-like? '(1 1 2 3 5 8 13 21))
(fib-like? '(2 1 3 4 7 11 18 29 47))

;3
(define (next-fib a b)
  (stream-cons a (next-fib b (+ a b))))
(define in-fib (next-fib 1 1))

(for/list ([i 10]
           [f in-fib])
  f)

;4
(define (next-feb x y)
  (yield x)
  (next-feb y (+ x y)))
(define gen-fib (generator () (next-feb 0 1)))

(gen-fib)
(gen-fib)
