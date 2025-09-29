#lang racket

(define (nats-from n)
  (stream-cons n (nats-from (+ n 1))))
(define naturals-stream (nats-from 1))

;1
(define (my-stream-map f s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (f (stream-first s)) (my-stream-map f (stream-rest s)))))

(define (my-stream-take s n)
  (if (or (<= n 0) (stream-empty? s))
      empty-stream
      (stream-cons (stream-first s) (my-stream-take (stream-rest s) (- n 1)))))

(stream->list (my-stream-take (my-stream-map (lambda (x) (* x x)) naturals-stream) 10))

;2
; Предположение индукции:
; (stream-take (stream-map f s) n) = (stream-map f (stream-take s n))
;
; 1. n=0 => empty-stream === empty-stream
;
; 2. n=k, list empty =>
; Левая часть:
;  (stream-take (stream-map f empty-stream) (k+1)) = (stream-take empty-stream (k+1)) = empty-stream
; Правая часть:
;  (stream-map f (stream-take empty-stream (k+1))) = (stream-map f empty-stream) = empty-stream
;
; 3. n=k+1 =>
; Левая часть:
;  (stream-take (stream-map f (stream-cons x s’)) (k+1))      |
;  (stream-take (stream-cons (f x) (stream-map f s’)) (k+1))  |
;  (stream-cons (f x) (stream-take (stream-map f s’) k))      |
;                                                             |=> по предположению индукции:
; Правая часть:                                               |   (stream-take (stream-map f s) n) = (stream-map f (stream-take s n)) = lst
;  (stream-map f (stream-take (stream-cons x s’) (k+1)))      |   Левая часть === Правая часть, тк
;  (stream-map f (stream-cons x (stream-take s’ k)))          |   (stream-cons (f x) lst) === (stream-cons (f x) lst)
;  (stream-cons (f x) (stream-map f (stream-take s’ k)))      |
;
