#lang racket
(require minikanren)
(require minikanren/numbers)

(define-syntax-rule (defrel (name x ...) g ...)
  (define (name x ...)
    (fresh () g ...)))

(defrel (memberᵒ x l)
  (conde
    [(fresh (h t)
       (== `(,h . ,t) l)
       (== h x))]
    [(fresh (h t)
       (== `(,h . ,t) l)
       (memberᵒ x t))]))

;; 3.1 КОНТЕКСТЫ

;; Упражнение 3.1: Поиск типа переменной в контексте
(defrel (context-lookupᵒ context var type)
  (memberᵒ `(,var . ,type) context))

;; Упражнение 3.2: Извлечение всех переменных из контекста
(defrel (context-varsᵒ context vars)
  (conde
    [(== '() context) (== '() vars)]
    [(fresh (var type rest-ctx rest-vars)
       (== `((,var . ,type) . ,rest-ctx) context)
       (== `(,var . ,rest-vars) vars)
       (context-varsᵒ rest-ctx rest-vars))]))

(define sample-context
  '((x . Int)
    (f . (Int -> Bool))
    (y . Int)
    (b . Bool)
    (+ . (Int -> (Int -> Int)))))

;; 3.2 БЕСТИПОВЫЕ ВЫРАЖЕНИЯ

;; Упражнение 3.3: Проверка валидности бестипового выражения
(defrel (untyped-exprᵒ vars expr)
  (conde
    [(symbolo expr) (memberᵒ expr vars)]
    [(numbero expr) (memberᵒ expr vars)]
    [(fresh (e1 op e2)
       (== `(,e1 ,op ,e2) expr)
       (conde [(== op '+)] [(== op '*)])
       (untyped-exprᵒ vars e1)
       (untyped-exprᵒ vars e2))]
    [(fresh (f x)
       (== `(,f ,x) expr)
       (untyped-exprᵒ vars f)
       (untyped-exprᵒ vars x))]
    [(fresh (e1 e2 e3)
       (== `(if ,e1 then ,e2 else ,e3) expr)
       (untyped-exprᵒ vars e1)
       (untyped-exprᵒ vars e2)
       (untyped-exprᵒ vars e3))]))

;; Упражнение 3.4: Выражения с ограниченной глубиной
(defrel (bounded-untyped-exprᵒ max-depth vars expr)
  (fresh ()
    ;; poso означает, что число > 0. Это позволяет базовым случаям (глубина 1) работать.
    (poso max-depth)
    
    (conde
      ;; Базовые случаи (глубина 1)
      [(symbolo expr) (memberᵒ expr vars)]
      [(numbero expr) (memberᵒ expr vars)]
      
      ;; Рекурсивные случаи
      [(fresh (sub-depth)
         ;; sub-depth = max-depth - 1
         (pluso (build-num 1) sub-depth max-depth)
         
         (conde
           [(fresh (e1 op e2)
              (== `(,e1 ,op ,e2) expr)
              (conde [(== op '+)] [(== op '*)])
              (bounded-untyped-exprᵒ sub-depth vars e1)
              (bounded-untyped-exprᵒ sub-depth vars e2))]
           
           [(fresh (f x)
              (== `(,f ,x) expr)
              (bounded-untyped-exprᵒ sub-depth vars f)
              (bounded-untyped-exprᵒ sub-depth vars x))]
           
           [(fresh (e1 e2 e3)
              (== `(if ,e1 then ,e2 else ,e3) expr)
              (bounded-untyped-exprᵒ sub-depth vars e1)
              (bounded-untyped-exprᵒ sub-depth vars e2)
              (bounded-untyped-exprᵒ sub-depth vars e3))]))])))

;; 3.3 ПРОВЕРКА И ВЫВОД ТИПОВ

;; Упражнение 3.5: Отношение типизации
(defrel (typed-exprᵒ context expr type)
  (conde
    ;; 1. Переменная
    [(symbolo expr)
     (context-lookupᵒ context expr type)]
    
    ;; 2. Число
    [(numbero expr)
     (context-lookupᵒ context expr type)]
    
    ;; 3. Арифметика
    [(fresh (e1 op e2)
       (== `(,e1 ,op ,e2) expr)
       (conde [(== op '+)] [(== op '*)])
       (== type 'Int)
       (typed-exprᵒ context e1 'Int)
       (typed-exprᵒ context e2 'Int))]
    
    ;; 4. Применение функции
    [(fresh (f arg arg-type)
       (== `(,f ,arg) expr)
       (typed-exprᵒ context f `(,arg-type -> ,type))
       (typed-exprᵒ context arg arg-type))]
    
    ;; 5. Условное выражение
    [(fresh (e1 e2 e3)
       (== `(if ,e1 then ,e2 else ,e3) expr)
       (typed-exprᵒ context e1 'Bool)
       (typed-exprᵒ context e2 type)
       (typed-exprᵒ context e3 type))]))

;; ТЕСТЫ

(displayln "--- 3.1 Context Lookup ---")
(run* (type) (context-lookupᵒ sample-context 'x type))
(run* (type) (context-lookupᵒ sample-context 'z type))
(run* (var) (context-lookupᵒ sample-context var 'Int))
(run* (var type) (context-lookupᵒ sample-context var type))

(displayln "\n--- 3.2 Context Vars ---")
(run* (vars) (context-varsᵒ sample-context vars))

(displayln "\n--- 3.3 Untyped Expressions ---")
(displayln "Генерация выражений из (x):")
(run 6 (expr) (untyped-exprᵒ '(x) expr))

(displayln "Генерация выражений из (x y):")
(run 8 (expr) (untyped-exprᵒ '(x y) expr))

(displayln "Вывод контекста для выражения:")
(run 1 (vars) (untyped-exprᵒ vars
  '(if (p x) then (x * (f (x + 1))) else (f (f x)))))

(displayln "\n--- 3.4 Bounded Untyped Expr ---")
(displayln "Выражения глубины <= 2:")
;; Используем встроенный build-num из библиотеки
(run* (expr) (bounded-untyped-exprᵒ (build-num 2) '(x) expr))

(displayln "Контекст для сложного выражения (ограниченная глубина):")
(run 1 (vars) (bounded-untyped-exprᵒ (build-num 5) vars
  '(if (p x) then (x * (f (x + 1))) else (f (f x)))))

(displayln "\n--- 3.5 Typed Expressions (Type Checking) ---")
(displayln "Проверка: if (f x) then (x + y) else y :: Int")
(run* (q) (typed-exprᵒ sample-context '(if (f x) then (x + y) else y) 'Int))

(displayln "Проверка ошибки:")
(run* (q) (typed-exprᵒ sample-context '(if (f x) then (x + b) else y) 'Int))

(displayln "\n--- 3.5 Type Inference ---")
(displayln "Вывод типа:")
(run* (type) (typed-exprᵒ
  `((1 . Int) . ,sample-context)
  '(if (f x) then (x + 1) else y)
  type))

(displayln "\n--- 3.5 Context Inference ---")
(run 1 (context) (typed-exprᵒ context '(if (f x) then (x + b) else y) 'Int))
(run 1 (context) (typed-exprᵒ context '((f (g x)) + (g (f x))) 'Int))
(run 1 (context type) (typed-exprᵒ context '(if (p x) then (f x) else x) type))

(displayln "\n--- 3.5 Program Synthesis ---")
(displayln "Синтез Int:")
(run 6 (expr) (typed-exprᵒ sample-context expr 'Int))
(displayln "Синтез Bool:")
(run 6 (expr) (typed-exprᵒ sample-context expr 'Bool))
(displayln "Синтез (Int -> Int):")
(run 6 (expr) (typed-exprᵒ sample-context expr '(Int -> Int)))