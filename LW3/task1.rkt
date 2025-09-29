#lang racket

;1
(define (ascending? lst)
  (or (null? lst)
      (for/and ([x lst]
                [y (cdr lst)])
        (< x y))))
(ascending? '(1 3 4 6 7 5 8 9))
(ascending? '(1 3 4 6 7 7 8 9))
(ascending? '(1 3 4 5 6 7 8 9))

;2
(define (moving-sum-3 lst)
  (if (< (length lst) 3)
      '()
      (for/list ([a lst]
                 [b (cdr lst)]
                 [c (cddr lst)])
        (+ a b c))))
(moving-sum-3 '(1 2 3 4 5 6 7))

;3
(define (choose-2 lst)
  (apply append
         (for/list ([i (in-range (length lst))]
                    [elem lst])
           (map (lambda (x) (cons elem x)) (drop lst (add1 i))))))
(choose-2 '(a b c d))

;4
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (coprime? lst)
  (andmap (lambda (pair) (= (gcd (car pair) (cdr pair)) 1)) (choose-2 lst)))

(coprime? '(2 3 5 7 9))
(coprime? '(8 9 35 11))

;5
(define (choose lst k)
  (cond
    [(= k 0) '(())]
    [(null? lst) '()]
    [else
     (append (map (lambda (comb) (cons (car lst) comb)) (choose (cdr lst) (- k 1)))
             (choose (cdr lst) k))]))
(choose '(a b c d e) 3)
