#lang racket

;1 Не работает
(define (second-to-last lst)
  (if (or (empty? lst) (empty? (rest lst)))
      (error "Список слишком короткий")
      (car (foldr (lambda (x acc)
                    (cond
                      [(empty? acc) (list x '())]
                      [(empty? (rest acc)) (list (car acc) x)]
                      [else (cons (car acc) (cdr acc))]))
                  '()
                  lst))))

;(second-to-last '(a b c d))

;2
(define (my-reverse lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))
;(my-reverse '(a b c d))

;3
(define (my-flatten lst)
  (cond
    [(list? lst)
     (apply append (map my-flatten lst))]
    [else (list lst)]))
;(my-flatten '(a (b (c d) e)))

;4
