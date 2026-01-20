#lang racket
(require minikanren)
(require minikanren/numbers)

(define-syntax-rule (defrel (name x ...) g ...)
  (define (name x ...)
    (fresh () g ...)))

(defrel (appendᵒ l1 l2 out)
  (conde
    [(== '() l1) (== l2 out)]
    [(fresh (h t res)
       (== `(,h . ,t) l1)
       (== `(,h . ,res) out)
       (appendᵒ t l2 res))]))

(defrel (memberᵒ x l)
  (conde
    [(fresh (head tail)
       (== `(,head . ,tail) l)
       (== head x))]
    [(fresh (head tail)
       (== `(,head . ,tail) l)
       (memberᵒ x tail))]))

(defrel (replicateᵒ n one many)
  (project (n)
    (cond
      [(not (var? n))
       (conde
        [(zeroo n) (== many '())]
        [(poso n)
         (fresh (n-1 tail)
                (minuso n (build-num 1) n-1)
                (appendᵒ one tail many)
                (replicateᵒ n-1 one tail))])]
      [else
       (conde
        [(== many '()) (zeroo n)]
        [(fresh (n-1 tail)
                (appendᵒ one tail many)
                (replicateᵒ n-1 one tail)
                (pluso n-1 (build-num 1) n))])])))

(defrel (every-word-nonemptyᵒ words)
  (conde
    [(== words '())]
    [(fresh (w ws)
       (== words (cons w ws))
       (fresh (a d) (== w (cons a d)))
       (every-word-nonemptyᵒ ws))]))

(defrel (wordsᵒ sentence words)
  (every-word-nonemptyᵒ words)
  (conde
    [(== sentence '())]
    [(fresh (w rest)
       (memberᵒ w words)
       (appendᵒ w rest sentence)
       (wordsᵒ rest words))]))

; tests
(displayln "--- 1.4 replicateᵒ ---")
(run* (q) (replicateᵒ (build-num 3) '(a b c) q))
(run* (q) (replicateᵒ (build-num 3) q '(a b c a b c a b c)))
(run* (q) (replicateᵒ q '(a b c) '(a b c a b c a b c)))

(displayln "\n--- 1.5 wordsᵒ ---")
(run* (q) (wordsᵒ '(a h a t h e h a d) '((h e) () (h a d) (a) (h a t))))
(run* (q) (wordsᵒ '(a h a t h e h a d) '((h e) (h a d) (h a t))))