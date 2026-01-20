#lang racket
(require minikanren)

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

(defrel (prefixᵒ p s)
  (fresh (tail)
    (appendᵒ p tail s)))

(defrel (plus-oneᵒ n m)
  (conde
    [(== '() n) (== '(1) m)]
    [(fresh (d)
       (== `(0 . ,d) n)
       (== `(1 . ,d) m))]
    [(fresh (d res)
       (== `(1 . ,d) n)
       (== `(0 . ,res) m)
       (plus-oneᵒ d res))]))

(define (build-num n)
  (cond
    [(zero? n) '()]
    [(even? n) (cons 0 (build-num (/ n 2)))]
    [(odd? n)  (cons 1 (build-num (/ (- n 1) 2)))]))

;; УПРАЖНЕНИЕ 14.1

;; 1. Вставка
(defrel (insertᵒ x xs after)
  (conde
    [(== after `(,x . ,xs))]
    [(fresh (h t res)
       (== xs `(,h . ,t))
       (== after `(,h . ,res))
       (insertᵒ x t res))]))

;; 2. Удаление элемента
(defrel (rembero x l out)
  (conde
    [(fresh (d)
       (== `(,x . ,d) l)
       (== d out))]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (=/= a x)
       (rembero x d res))]))

;; Анаграмма
(defrel (anagramᵒ x y)
  (conde
    [(== '() x) (== '() y)]
    [(fresh (h t y-rem)
       (== `(,h . ,t) x)
       (rembero h y y-rem)
       (anagramᵒ t y-rem))]))

;; 3. Подпоследовательность
(defrel (subseqᵒ sub main)
  (conde
    [(== '() sub)]
    [(fresh (x sub-tail main-tail)
       (== `(,x . ,sub-tail) sub)
       (== `(,x . ,main-tail) main)
       (subseqᵒ sub-tail main-tail))]
    [(fresh (h main-tail)
       (== `(,h . ,main-tail) main)
       (subseqᵒ sub main-tail))]))

;; 4. Поиск
(defrel (searchᵒ needle haystack pos)
  (conde
    [(prefixᵒ needle haystack)
     (== '() pos)]
    [(fresh (h t prev-pos)
       (== `(,h . ,t) haystack)
       (searchᵒ needle t prev-pos)
       (plus-oneᵒ prev-pos pos))]))

;; УПРАЖНЕНИЕ 14.2 

;; 1. Не префикс
(defrel (not-prefixᵒ p s)
  (conde
    ;; p не пустой, а s пустой
    [(fresh (ph pt)
       (== `(,ph . ,pt) p)
       (== '() s))]
    ;; Головы отличаются (используем =/=)
    [(fresh (ph pt sh st)
       (== `(,ph . ,pt) p)
       (== `(,sh . ,st) s)
       (=/= ph sh))]
    ;; Головы совпадают, рекурсивно проверяем хвосты
    [(fresh (ph pt sh st)
       (== `(,ph . ,pt) p)
       (== `(,sh . ,st) s)
       (== ph sh)
       (not-prefixᵒ pt st))]))

;; 2. Не подсписок
(defrel (not-sublistᵒ sub main)
  (conde
    ;; Если main пустой, sub должен быть не пустым
    [(== '() main)
     (fresh (h t) (== `(,h . ,t) sub))]
    ;; Если main не пустой:
    [(fresh (h t)
       (== `(,h . ,t) main)
       ;; 1. sub не является префиксом здесь
       (not-prefixᵒ sub main)
       ;; 2. И sub не встречается в хвосте
       (not-sublistᵒ sub t))]))

;; УПРАЖНЕНИЕ 14.3 (Замена)

;; 1. Замена
(defrel (replaceᵒ old new whole result)
  (conde
    [(== '() whole) (== '() result)]
    [(fresh (whole-rest res-rest)
       (appendᵒ old whole-rest whole)
       (appendᵒ new res-rest result)
       (replaceᵒ old new whole-rest res-rest))]
    [(fresh (h whole-tail res-tail)
       (== `(,h . ,whole-tail) whole)
       (== `(,h . ,res-tail) result)
       (replaceᵒ old new whole-tail res-tail))]))

;; 2. Замена всех
(defrel (replace-allᵒ old new whole result)
  (fresh ()
    (replaceᵒ old new whole result)
    (not-sublistᵒ old result)))

;; ТЕСТЫ

(displayln "--- 14.1 Insert ---")
(run* (q) (fresh (x xs) (== q `(,x ,xs)) (insertᵒ x xs '(1 2 3))))
(run* (q) (fresh (x xs) (== q `(,x ,xs)) (insertᵒ x xs '(a b a c))))

(displayln "\n--- 14.1 Anagram ---")
(run 1 (q) (anagramᵒ '(d o r m i t o r y) '(d i r t y r o o m)))

(displayln "\n--- 14.1 Subseq ---")
(run* (q) (subseqᵒ '(2 4 5) '(1 2 3 4 5 6)))
(run* (xs) (subseqᵒ xs '(1 2 3)))
(run* (xs) (fresh (a b c) (== xs `(,a ,b ,c)) (subseqᵒ '(1 2) xs)))

(displayln "\n--- 14.1 Search ---")
(run* (pos) (searchᵒ '(a b a) '(c a b a b a d) pos))
(run* (needle) (searchᵒ needle '(c a b a b a d) (build-num 5)))
(run* (q) (fresh (needle pos x) (== needle `(a ,x a)) (== q `(,needle ,pos)) (searchᵒ needle '(c a b a b a d) pos)))

(displayln "\n--- 14.2 Not Prefix ---")
(run* (xs) (not-prefixᵒ '(a b) '(a b r a b a)))
(run* (xs) (not-prefixᵒ '(a b a) '(a b r a b a)))

(displayln "\n--- 14.2 Not Sublist ---")
(run* (xs) (not-sublistᵒ '(a b a) '(a b r a b a)))
(run* (xs) (not-sublistᵒ '(a b c) '(a b r a b a)))

(displayln "\n--- 14.3 Replace ---")
(run* (new-whole) (replaceᵒ '(a b) '(N E W) '(a b r a b a) new-whole))
(run* (new-whole) (replaceᵒ '(a a) '(x y) '(a a a a) new-whole))

(displayln "\n--- 14.3 Replace All ---")
(run* (new-whole) (replace-allᵒ '(a b) '(N E W) '(a b r a b a) new-whole))
(run* (new-whole) (replace-allᵒ '(a a) '(x y) '(a a a a) new-whole))