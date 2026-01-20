#lang racket
(require minikanren)

(define-syntax-rule (defrel (name x ...) g ...)
  (define (name x ...)
    (fresh () g ...)))

(defrel (studentᵒ name group)
  (conde
    [(== name 'Дарья) (== group 2)]
    [(== name 'Максим) (== group 1)]
    [(== name 'Кирилл) (== group 2)]
    [(== name 'Александр) (== group 1)]
    [(== name 'Владимир) (== group 2)]))

(defrel (friendᵒ name1 name2)
  (conde
    [(== name1 'Дарья) (== name2 'Максим)]
    [(== name1 'Дарья) (== name2 'Александр)]
    [(== name1 'Максим) (== name2 'Кирилл)]
    [(== name1 'Максим) (== name2 'Владимир)]
    [(== name1 'Кирилл) (== name2 'Александр)]
    [(== name1 'Александр) (== name2 'Владимир)]))

(defrel (parentᵒ parent-name child-name)
  (conde
    [(== parent-name 'Мардж) (== child-name 'Барт)]
    [(== parent-name 'Мардж) (== child-name 'Лиза)]
    [(== parent-name 'Мардж) (== child-name 'Мэгги)]
    [(== parent-name 'Гомер) (== child-name 'Барт)]
    [(== parent-name 'Гомер) (== child-name 'Лиза)]
    [(== parent-name 'Гомер) (== child-name 'Мэгги)]
    [(== parent-name 'Абрахам) (== child-name 'Гомер)]
    [(== parent-name 'Мона) (== child-name 'Гомер)]
    [(== parent-name 'Жаклин) (== child-name 'Мардж)]
    [(== parent-name 'Жаклин) (== child-name 'Пэтти)]
    [(== parent-name 'Жаклин) (== child-name 'Сельма)]
    [(== parent-name 'Клэнси) (== child-name 'Мардж)]
    [(== parent-name 'Клэнси) (== child-name 'Пэтти)]
    [(== parent-name 'Клэнси) (== child-name 'Сельма)]
    [(== parent-name 'Сельма) (== child-name 'Линг)]))

(defrel (unaryᵒ n)
  (conde
    [(== 'z n)]
    [(fresh (m)
       (== `(s ,m) n)
       (unaryᵒ m))]))

(defrel (direct-trainᵒ from to)
  (conde
    [(== from 'Мытищи) (== to 'Химки)]
    [(== from 'Люберцы) (== to 'Мытищи)]
    [(== from 'Одинцово) (== to 'Люберцы)]
    [(== from 'Красногорск) (== to 'Одинцово)]
    [(== from 'Балашиха) (== to 'Красногорск)]
    [(== from 'Видное) (== to 'Балашиха)]
    [(== from 'Коломна) (== to 'Видное)]))

(defrel (memberᵒ x l)
  (conde
    [(fresh (d) (== `(,x . ,d) l))]
    [(fresh (a d) (== `(,a . ,d) l) (memberᵒ x d))]))

(defrel (addᵒ x y z)
  (conde
    [(== 'z x) (== y z)]
    [(fresh (x-1 z-1)
       (== `(s ,x-1) x)
       (== `(s ,z-1) z)
       (addᵒ x-1 y z-1))]))

;; УПРАЖНЕНИЕ 13.1

(defrel (groupmatesᵒ student1 student2)
  (fresh (g)
    (studentᵒ student1 g)
    (studentᵒ student2 g)))

(defrel (ancestorᵒ a child)
  (conde
    [(parentᵒ a child)]
    [(fresh (p)
       (parentᵒ p child)
       (ancestorᵒ a p))]))

(defrel (relativeᵒ x y)
  (fresh (a)
    (ancestorᵒ a x)
    (ancestorᵒ a y)))

;; УПРАЖНЕНИЕ 13.2

(defrel (binaryᵒ l)
  (conde
    [(== '() l)]
    [(fresh (a d)
       (== `(,a . ,d) l)
       (conde
         [(== a 0)]
         [(== a 1)])
       (binaryᵒ d))]))

(defrel (wordᵒ chars word)
  (conde
    [(== '() word)]
    [(fresh (a d)
       (== `(,a . ,d) word)
       (memberᵒ a chars)
       (wordᵒ chars d))]))

;; УПРАЖНЕНИЕ 13.3

(defrel (doubleᵒ x y)
  (addᵒ x x y))

(defrel (leqᵒ x y)
  (conde
    [(== 'z x)]
    [(fresh (x-1 y-1)
       (== `(s ,x-1) x)
       (== `(s ,y-1) y)
       (leqᵒ x-1 y-1))]))

(defrel (multᵒ x y z)
  (conde
    [(== 'z x) (== 'z z)]
    [(fresh (x-1 res)
       (== `(s ,x-1) x)
       (multᵒ x-1 y res)
       (addᵒ res y z))]))

(defrel (power-of-2ᵒ n m)
  (conde
    [(== 'z n) (== '(s z) m)]
    [(fresh (n-1 res)
       (== `(s ,n-1) n)
       (doubleᵒ res m)
       (leqᵒ res m)
       (power-of-2ᵒ n-1 res))]))

;; УПРАЖНЕНИЕ 13.4

(defrel (trainᵒ from to)
  (conde
    [(direct-trainᵒ from to)]
    [(fresh (stop)
       (direct-trainᵒ from stop)
       (trainᵒ stop to))]))

(defrel (train-pathᵒ from to path)
  (conde
    [(direct-trainᵒ from to)
     (== path `(,from ,to))]
    [(fresh (stop tail-path)
       (direct-trainᵒ from stop)
       (train-pathᵒ stop to tail-path)
       (== path `(,from . ,tail-path)))]))

;; ТЕСТЫ

(displayln "--- 13.1 Groupmates ---")
(displayln "Дарья и Максим:")
(run 1 (q) (groupmatesᵒ 'Дарья 'Максим))
(displayln "Дарья и Владимир:")
(run 1 (q) (groupmatesᵒ 'Дарья 'Владимир))

(displayln "\n--- 13.1 Relatives ---")
(displayln "Сельма и Пэтти:")
(run 1 (q) (relativeᵒ 'Сельма 'Пэтти))
(displayln "Лиза и Линг:")
(run 1 (q) (relativeᵒ 'Лиза 'Линг))
(displayln "Гомер и Сельма:")
(run 1 (q) (relativeᵒ 'Гомер 'Сельма))

(displayln "\n--- 13.2 Binary ---")
(displayln "Заполнение пропуска (1 _ 0):")
(run* (x) (binaryᵒ `(1 ,x 0)))
(displayln "Генерация бинарных списков:")
(run 6 (xs)
  (fresh (ys) (== `(1 . ,ys) xs))
  (binaryᵒ xs))

(displayln "\n--- 13.2 Words ---")
(displayln "Генерация слов из алфавита (a b):")
(run* (q)
  (fresh (x y z)
    (== q `(,x ,y ,z))
    (wordᵒ '(a b) `(,x ,y ,z))))

(displayln "\n--- 13.3 Unary Numbers ---")
(displayln "Удвоение 2 (s s z):")
(run 1 (q) (doubleᵒ '(s (s z)) q))
(displayln "Из чего получается 4 при удвоении?:")
(run 1 (x) (doubleᵒ x '(s (s (s (s z))))))

(displayln "2 <= 3 ?")
(run 1 (q) (leqᵒ '(s (s z)) '(s (s (s z)))))
(displayln "4 <= 3 ?")
(run 1 (q) (leqᵒ '(s (s (s (s z)))) '(s (s (s z)))))

(displayln "Умножение 2 * 3:")
(run 1 (x) (multᵒ '(s (s z)) '(s (s (s z))) x))
(displayln "Деление 6 / 2:")
(run 1 (x) (multᵒ x '(s (s z)) '(s (s (s (s (s (s z))))))))

(displayln "Степень двойки (2^2):")
(run 1 (q) (power-of-2ᵒ '(s (s z)) q))
(displayln "Логарифм (2^x = 4):")
(run 1 (x) (power-of-2ᵒ x '(s (s (s (s z))))))
(displayln "Является ли 3 степенью двойки?:")
(run 1 (x) (power-of-2ᵒ x '(s (s (s z)))))

(displayln "\n--- 13.4 Trains ---")
(displayln "Есть ли поезд Люберцы -> Химки?:")
(run 1 (q) (trainᵒ 'Люберцы 'Химки))
(displayln "Куда можно доехать из Люберец?:")
(run* (q) (trainᵒ 'Люберцы q))
(displayln "Откуда можно доехать в Люберцы?:")
(run* (q) (trainᵒ q 'Люберцы))

(displayln "Маршрут Люберцы -> Химки:")
(run 1 (path) (train-pathᵒ 'Люберцы 'Химки path))
(displayln "Маршруты из Люберец куда-либо:")
(run* (path) (fresh (to) (train-pathᵒ 'Люберцы to path)))