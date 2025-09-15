#lang racket

;my-map
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (my-map f (cdr lst)))))
(my-map (lambda (x) (* x x)) '(1 2 3 4))

;my-filter
(define (my-filter pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (cons (car lst) (my-filter pred (cdr lst)))]
    [else (my-filter pred (cdr lst))]))
(my-filter even? '(1 2 3 4 5 6))

;my-andmap
(define (my-andmap pred lst)
  (cond
    [(null? lst) #t]
    [(not (pred (car lst))) #f]
    [else (my-andmap pred (cdr lst))]))
(my-andmap positive? '(1 2 3))
(my-andmap positive? '(1 -2 3))

;my-ormap
(define (my-ormap pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (my-ormap pred (cdr lst))]))
(my-ormap even? '(1 3 5 6))
(my-ormap even? '(1 3 5))

;my-foldl
(define (my-foldl f acc lst)
  (if (null? lst)
      acc
      (my-foldl f (f (car lst) acc) (cdr lst))))
(my-foldl + 0 '(1 2 3 4))
(my-foldl (lambda (x acc) (cons x acc)) '() '(a b c))

;1
