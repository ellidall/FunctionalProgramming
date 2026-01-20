#lang racket

(define (stream-chunks s n)
  (if (stream-empty? s)
      empty-stream
      (stream-cons
       (for/list ([item s]
                  [i (in-range n)])
         item)
       (stream-chunks (stream-tail s n) n))))

(define (stream-tail s n)
  (if (or (zero? n) (stream-empty? s))
      s
      (stream-tail (stream-rest s) (sub1 n))))

; test
(for/list ([i 3]
           [chunk (stream-chunks (in-naturals 1) 4)])
  chunk)



(define (stream-splits lst)
  (let loop ([left-rev '()]
             [right lst])
    (stream-cons
     (cons (reverse left-rev) right)
     (if (null? right)
         empty-stream
         (loop (cons (car right) left-rev)
               (cdr right))))))

; test
(stream->list (stream-splits '(a b c)))