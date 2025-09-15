#lang racket

;1
(define (second-to-last lst)
  (if (or (empty? lst) (empty? (rest lst)))
      (error "Список слишком короткий")
      (car (foldr (lambda (x acc)
                    (cond
                      [(empty? acc) (list x 1)]
                      [(= (cadr acc) 2) acc]
                      [else (list x (+ (cadr acc) 1))]))
                  '()
                  lst))))
(second-to-last '(a b c d))



;2
(define (my-reverse lst)
  (foldl cons '() lst))
(my-reverse '(a b c d))



;3
(define (my-flatten lst)
  (cond
    [(list? lst)
     (apply append (map my-flatten lst))]
    [else (list lst)]))
(my-flatten '(a (b (c d) e)))



;4
(define (my-encode lst)
  (foldr
   (lambda (x acc)
     (if (null? acc)
         (list (list x 1))
         (let ((head (car acc)))
           (if (equal? (car head) x)
               (cons (list x (+ 1 (cadr head))) (cdr acc))
               (cons (list x 1) acc)))))
   '()
   lst))
(my-encode '(a a a a b c c a a d e e e e))



;5
(define (my-decode encoded)
  (apply append
         (map (lambda (pair)
                (make-list (cadr pair) (car pair)))
              encoded)))
(my-decode '((a 4) (b 1) (c 2) (a 2) (d 1) (e 4)))



;6
(define (my-split lst n)
  (cond
    [(< n 0) (error "Индекс не может быть отрицательным")]
    [else
     (let ((result (foldl (lambda (x acc)
                           (let ((count (car acc))
                                 (left (cadr acc))
                                 (right (caddr acc)))
                             (if (< count n)
                                 (list (+ count 1) (cons x left) right)
                                 (list (+ count 1) left (cons x right)))))
                         (list 0 '() '())
                         lst)))
       (list (reverse (cadr result)) (reverse (caddr result))))]))
(my-split '(a b c d e f g h i k) 3)