#lang racket
(require slideshow)

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
(slide (para #:align 'left
             (t "(define (map f lst)")
             (t "  (if (null? lst)")
             (t "    '()")
             (t "    (cons (f (car lst))")
             (t "         (map f (cdr lst)))))")
             (bt "Доказательство: (map (λ(x) x) lst) = lst")
             (t "Базовый случай: lst = '()")
             (t "(map (λ(x) x) '()) = '()")
             (blank 10)
             (t "Индукционный шаг: lst = (cons a b)")
             (t "(map (λ(x) x) (cons a b))")
             (t "= (cons ((lambda (x) x) (car (cons a b)))")
             (t "        (map (lambda (x) x) (cdr (cons a b))))")
             (t "= (cons a (map (λ(x) x) b))")
             (t "= (cons a b)  [по предположению индукции]")
             (blank 10)
             (bt "Пример: (map (λ(x) x) '(1 2))")
             (t " (cons 1 (map (λ(x) x) '(2)))")
             (t " (cons 1 (cons 2 (map (λ(x) x) '())))")
             (t " (cons 1 (cons 2 '())) → '(1 2)")))

; 2
; (map f (map g lst)) = (map (lambda (x) (f (g x))) lst)
; Пусть метод map уже определен в прошлом задании. Докажем методом мат индукции.
;
; 1. Исходный массив пустой. Тогда
; (map f (map g '()))
; = (map f '())                                           
; = '()
;
; (map (lambda (x) (f (g x))) '())
; = '()
;
; 2. Пусть верно, что (map f (map g cdr-lst)) = (map (lambda (x) (f (g x))) cdr-lst)
;
; Докажем, что верно и (map f (map g (cons a cdr-lst))) = (map (lambda (x) (f (g x))) (cons a cdr-lst))
;
; (map f (map g (cons a cdr-lst)))
; = (map f (cons (g a) (map g cdr-lst)))
; = (cons (f (g a)) (map f (map g cdr-lst)))
; = (cons (f (g a)) (map (lambda (x) (f (g x))) cdr-lst))
;
; (map (lambda (x) (f (g x))) (cons a cdr-lst))
; = (cons ((lambda (x) (f (g x))) a) (map (lambda (x) (f (g x))) cdr-lst))
; = (cons (f (g a)) (map (lambda (x) (f (g x))) cdr-lst))
;
