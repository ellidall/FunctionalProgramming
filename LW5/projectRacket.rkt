#lang racket

(define a 1103515245)
(define c 12345)
(define m (expt 2 31))
(struct rng (state) #:transparent)

(displayln "----------------------Упражнение 1----------------------------")
(define (make-rng n)
  (rng (modulo n m)))
(define (next-rng g)
  (rng (modulo (+ (* a (rng-state g)) c) m)))

;testcase
(define gen (make-rng 123))
(next-rng (next-rng gen))

(displayln "----------------------Упражнение 2----------------------------")
(define (random-integer i j)
  (lambda (g)
    (let* ([g-next (next-rng g)]
           [x (rng-state g-next)]
           [len (- j i -1)])
      (cons (+ i (modulo x len)) g-next))))

;testcase
((random-integer 7 77) (make-rng 709))
((random-integer 7 77) (make-rng 709))

(displayln "----------------------Упражнение 3----------------------------")
(define (stream-random gen-fn g)
  (let ([result (gen-fn g)]) (stream-cons (car result) (stream-random gen-fn (cdr result)))))

;testcase
(stream->list (stream-take (stream-random (random-integer 1 10) (make-rng 709)) 5))
(stream->list (stream-take (stream-random (random-integer 1 10) (make-rng 709)) 5))

(displayln "----------------------Упражнение 4----------------------------")
(define (sample gen-fn [n 5] [init-state (make-rng 709)])
  (stream->list (stream-take (stream-random gen-fn init-state) n)))

;testcase
(sample (random-integer 1 10))
(sample (random-integer 1 10) 10)
(sample (random-integer 1 10) 10 (make-rng 705))

(displayln "----------------------Упражнение 5----------------------------")
(define (map-random f gen)
  (lambda (g) (let ([result (gen g)]) (cons (f (car result)) (cdr result)))))

;testcase
((random-integer 1 100) (make-rng 709))
((map-random even? (random-integer 1 100)) (make-rng 709))
(sample (random-integer 1 100))
(sample (map-random even? (random-integer 1 100)))

(displayln "----------------------Упражнение 6----------------------------")
(define random-bool (map-random even? (random-integer 0 1000000)))

;testcase
(sample random-bool 10)

(displayln "----------------------Упражнение 7----------------------------")
(define random-coin-flip (map-random (lambda (b) (if b 'орёл 'решка)) random-bool))

;testcase
(sample random-coin-flip 10)

(displayln "----------------------Упражнение 8----------------------------")
(define random-dice-roll (random-integer 1 6))

;testcase
(sample random-dice-roll 20)

(displayln "----------------------Упражнение 9----------------------------")
(define (frequency-count lst)
  (hash->list (for/fold ([counts (hash)]) ([x lst])
                (hash-update counts x add1 0))))

;testcase
(frequency-count (sample random-coin-flip 1000))
(sort (frequency-count (sample random-dice-roll 600)) (lambda (l r) (< (car l) (car r))))

(displayln "----------------------Упражнение 10----------------------------")
(define (random-cons gen1 gen2)
  (lambda (g)
    (let* ([res1 (gen1 g)]
           [res2 (gen2 (cdr res1))])
      (cons (cons (car res1) (car res2)) (cdr res2)))))

;testcase
(sample (random-cons (random-integer 1 10) (random-integer 1 10)))

(displayln "----------------------Упражнение 11----------------------------")
(define (random-if gen-cond gen-then gen-else)
  (lambda (g)
    (let* ([cond-res (gen-cond g)]
           [bool-val (car cond-res)]
           [g1 (cdr cond-res)])
      (if bool-val
          (gen-then g1)
          (gen-else g1)))))

;testcase
(sample (random-if random-bool random-dice-roll random-coin-flip))

(displayln "----------------------Упражнение 12----------------------------")
(define (random-constant val)
  (lambda (g) (cons val g)))

;testcase
(sample (random-constant 'ё) 10)

(displayln "----------------------Упражнение 13----------------------------")
(define (random-choice gen1 gen2)
  (lambda (g)
    (let* ([bool-res (random-bool g)]
           [choose-first? (car bool-res)]
           [g1 (cdr bool-res)])
      (if choose-first?
          (gen1 g1)
          (gen2 g1)))))

;testcase
(sample (random-choice random-dice-roll random-coin-flip))

(displayln "----------------------Упражнение 14----------------------------")
(define (random-item lst)
  (let ([n (length lst)])
    (unless (> n 0)
      (error "random-item: список не должен быть пустым"))
    (let ([index-gen (random-integer 0 (- n 1))])
      (lambda (g)
        (let ([idx-res (index-gen g)]) (cons (list-ref lst (car idx-res)) (cdr idx-res)))))))

;testcase
(sample (random-item '(a b c)) 10)

(displayln "----------------------Упражнение 15----------------------------")
(define (random-list-of-size gen n)
  (lambda (g)
    (let ([result (for/fold ([acc-state (cons '() g)]) ([i (in-range n)])
                    (let* ([current-state (cdr acc-state)]
                           [res (gen current-state)]
                           [value (car res)]
                           [new-state (cdr res)])
                      (cons (cons value (car acc-state)) new-state)))])
      (cons (reverse (car result)) (cdr result)))))

;testcase
(sample (random-list-of-size random-coin-flip 2))
(frequency-count (sample (random-list-of-size random-coin-flip 2) 1000))

(displayln "----------------------Упражнение 16----------------------------")
(define (random-bind gen f)
  (lambda (g) (let ([res (gen g)]) ((f (car res)) (cdr res)))))

;testcase
(sample (random-bind random-dice-roll (lambda (n) (random-list-of-size random-coin-flip n))))

(displayln "----------------------Упражнение 17----------------------------")
(define (random-list gen)
  (random-bind (random-integer 0 10) (lambda (n) (random-list-of-size gen n))))

;testcase
(sample (random-list random-bool) 1)

(displayln "----------------------Упражнение 18----------------------------")
(define-syntax let*/random
  (syntax-rules ()
    [(_ ([var gen]) body) (random-bind gen (lambda (var) (random-constant body)))]
    [(_ ([var gen] rest ...) body) (random-bind gen (lambda (var) (let*/random (rest ...) body)))]))

;testcase
(sample (let*/random ([n random-dice-roll] [lst (random-list-of-size random-coin-flip n)])
                     (length lst))
        20)

(displayln "----------------------Упражнение 19----------------------------")
(define (random-real a b)
  (lambda (g)
    (let* ([g-next (next-rng g)]
           [x (rng-state g-next)]
           [fraction (/ (exact->inexact x) (exact->inexact m))]
           [value (+ a (* (- b a) fraction))])
      (cons value g-next))))

;testcase
(sample (random-real 0 1))

(displayln "----------------------Упражнение 20----------------------------")
(define random-point-in-unit-square (random-cons (random-real -1 1) (random-real -1 1)))

;testcase
(sample random-point-in-unit-square)

(displayln "----------------------Упражнение 21----------------------------")
(define (monte-carlo-pi n)
  (let* ([points (sample random-point-in-unit-square n)]
         [inside (length (filter (lambda (p) (<= (+ (sqr (car p)) (sqr (cdr p))) 1)) points))])
    (* 4.0 (/ inside n))))

;testcase
(monte-carlo-pi 100)
(monte-carlo-pi 1000)
(monte-carlo-pi 10000)

(struct property (random-value predicate))





































; (displayln "----------------------Упражнение 22----------------------------")
; (define random-triple-of-reals
;   (random-cons (random-real 0 1) (random-cons (random-real 0 1) (random-real 0 1))))

; (define real-addition-is-associative
;   (property (random-cons (random-real 0 1) (random-cons (random-real 0 1) (random-real 0 1)))
;             (lambda (triple)
;               (let ([x (car triple)]
;                     [y (car (cdr triple))]
;                     [z (cdr (cdr triple))])
;                 (= (+ x (+ y z)) (+ (+ x y) z))))))
; ;testcase

; (displayln "----------------------Упражнение 23----------------------------")
; (define (find-counterexample prop)
;   (let ([gen (property-random-value prop)]
;         [pred (property-predicate prop)]
;         [tries 100])
;     (let loop ([i 0]
;                [state (make-rng 0)])
;       (cond
;         [(>= i tries) '()]
;         [else
;          (let ([res (gen state)])
;            (if (pred (car res))
;                (loop (+ i 1) (cdr res))
;                (list (car res))))]))))

; ;testcase
; (find-counterexample property-reverse-reverse)
; ; '() — не нашлись контрпримеры
; (find-counterexample real-addition-is-associative)
; ; '((0.546 0.357 0.148)) — есть как минимум один контрпример

; (displayln "----------------------Упражнение 24----------------------------")

; ;testcase
; (find-counterexample (forall/property ([x (random-real 0 1)] [y (random-real 0 1)]
;                                                              [z (random-real 0 1)])
;                                       (= (+ x (+ y z)) (+ (+ x y) z))))
; ; '(((z = 0.546) (y = 0.357) (x = 0.148)))

; (displayln "----------------------Упражнение 25----------------------------")

; ;testcase
; (find-counterexample
;  (forall*/property ([x (random-real 0 1)] [y (random-real 0 x)] [z (random-real 0 y)]) (> x (+ y z))))
; ; '(((z = 0.1988745) (y = 0.530332) (x = 0.709)))

; (displayln "----------------------Упражнение 26----------------------------")

; ;testcase
