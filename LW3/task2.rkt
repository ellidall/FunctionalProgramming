#lang racket

;1
(define (trib-stream a b c)
  (stream-cons a (trib-stream b c (+ a b c))))
(define in-tribonacci (trib-stream 0 0 1))
(for/list ([i 10]
           [f in-tribonacci])
  f)

;2
(define (stream-takef strm pred)
  (if (stream-empty? strm)
      empty-stream
      (let ([x (stream-first strm)])
        (if (pred x)
            (stream-cons x (stream-takef (stream-rest strm) pred))
            empty-stream))))
(stream->list (stream-takef (in-naturals 1) (lambda (x) (< x 10))))

;3
(define (stream-take-list strm n)
  (if (or (<= n 0) (stream-empty? strm))
      '()
      (cons (stream-first strm) (stream-take-list (stream-rest strm) (- n 1)))))

(define (stream-drop strm n)
  (if (<= n 0)
      strm
      (stream-drop (stream-rest strm) (- n 1))))

(define (stream-chunks strm n)
  (let ([chunk (stream-take-list strm n)])
    (if (< (length chunk) n)
        empty-stream
        (stream-cons chunk (stream-chunks (stream-drop strm n) n)))))
(for/list ([i 3]
           [chunk (stream-chunks (in-naturals 1) 4)])
  chunk)

;4
(define (stream-zigzag s1 s2)
  (define (pairs-by-diagonal k i)
    (if (> i k)
        (pairs-by-diagonal (+ k 1) 0)
        (stream-cons (cons (stream-ref s1 i) (stream-ref s2 (- k i))) (pairs-by-diagonal k (+ i 1)))))
  (pairs-by-diagonal 0 0))
(for/list ([i 6]
           [x (stream-zigzag (in-naturals 1) (in-naturals 1))])
  x)
