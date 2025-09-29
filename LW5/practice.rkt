#lang racket
(require racket/generator)

;1
(define (palindrome? lst)
  (match lst
    [(list) #t]
    [(list x) #t]
    [(list x y ... x) (palindrome? y)]
    [_ #f]))
(palindrome? '(a b b a b a b b a))
(palindrome? '(a b b a))
(palindrome? '(a b b))
(palindrome? '())
(palindrome? '(a))
(palindrome? '(a a))
(palindrome? 'a)

;2
(define (cart-total lst)
  (match lst
    [(list) 0]
    [(cons (list title price amount) rest) (+ (* price amount) (cart-total rest))]))
(cart-total '((milk 119.99 3) (bread 89.99 2) (butter 189.99 1)))
