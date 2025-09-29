#lang racket
(require racket/generator)

;rand-gen
(define m 2147483648)
(define a 1103515245)
(define c 12345)
(define current-seed 12345)

;(define (rand)
;  (let ([next-seed (modulo (+ (* a current-seed) c) m)])
;    (set! current-seed next-seed)
;    next-seed))

;(define (rand-stream seed)
;  (stream-cons seed (rand-stream (modulo (+ (* a seed) c) m))))

;(define (rand-stream seed)
;  (let ([next-seed (modulo (+ (* a seed) c) m)]) (stream-cons next-seed (rand-stream next-seed))))

;(define rand (rand-stream current-seed))

(define (next-rand x)
  (let ([next (modulo (+ (* a x) c) m)])
    (yield next)
    (next-rand next)))

(define rand (generator () (next-rand current-seed)))

(for/list ([i 10]
           [x (in-producer rand)])
  x)
