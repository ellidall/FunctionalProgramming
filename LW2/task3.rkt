#lang racket

;1
(define (length-via-foldl lst)
  (foldl (lambda (element acc) (+ acc 1)) 0 lst))
(length-via-foldl '(a b c d e))

;2
(define (reverse-via-foldl lst)
  (foldl cons '() lst))
(reverse-via-foldl '(a b c d e))

;3
(define (map-via-foldl f lst)
  (foldl (lambda (x acc) (append acc (list (f x)))) '() lst))
(map-via-foldl (lambda (x) (* x x)) '(1 2 3 4 5))
