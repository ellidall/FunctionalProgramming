 #lang racket

(define (second-to-last lst)
  (cond
    [(or (empty? lst) (empty? (rest lst))) 
     (error "Список слишком короткий")]
    [(empty? (rest (rest lst))) 
     (first lst)]
    [else (second-to-last (rest lst))]))

;(second-to-last '(a b c d))



(define (my-reverse lst)
  (define (helper src acc)
    (if (empty? src)
        acc
        (helper (rest src) (cons (first src) acc))))
  (helper lst '()))

;(my-reverse '(a b c d))



(define (my-flatten lst)
  (cond
    [(empty? lst) '()]
    [(list? (first lst)) (append (my-flatten (first lst)) (my-flatten (rest lst)))]
    [else (cons (first lst) (my-flatten (rest lst)))]))

;(my-flatten '(a (b (c d) e)))



(define (my-encode lst)
  (define (helper current count res)
    (cond
      [(empty? res)
       (list (list current count))]
      [(equal? current (first res))
       (helper current (+ count 1) (rest res))]
      [else
       (cons (list current count)(helper (first res) 1 (rest res)))]))
  (if (empty? lst)
      (empty)
      (helper (first lst) 1 (rest lst))))
    
;(my-encode '(a a a a b c c a a d e e e e))


(define (my-decode lst)
  (define (helper elem n)
    (if (zero? n)
        '()
        (cons elem (helper elem (- n 1)))))
  (if (empty? lst)
      '()
      (append (helper (first (first lst)) (second (first lst)))
              (my-decode (rest lst)))))

;(my-decode '((a 4) (b 1) (c 2) (a 2) (d 1) (e 4)))



(define (my-split lst n)
  (define (helper current count res)
    (if (zero? count)
        (list current res) 
        (helper (append current (list (first res))) (- count 1) (rest res))))
  (helper '() n lst))

;(my-split '(a b c d e f g h i k) 3)





