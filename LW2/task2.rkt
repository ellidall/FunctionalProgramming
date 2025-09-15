#lang racket


;1
(define (gen-sized-list n start gen)
  (if (<= n 0)
      '()
      (cons start (gen-sized-list (- n 1) (gen start) gen))))

(gen-sized-list 5 0 (lambda (x) (+ x 1)))
(gen-sized-list 4 '(a b) reverse)

;2
(define (gen-list start gen)
  (let ((next (gen start)))
    (if (equal? next #f)
        '()            
        (cons start             
              (gen-list next gen)))))

(gen-list 10 (lambda (x) (if (< x 0) #f (- x 1))))

;3
(define (gen-range start end step)
  (if (>= start end)
      '()
      (cons start
            (gen-range (+ start step) end step))))
(gen-range 0 10 2)

