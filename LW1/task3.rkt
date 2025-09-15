#lang slideshow

;Для отрисовки штрихкода
(define (draw-bars sequence [width 20] [height 80])
  (if (empty? sequence)
      (rectangle 0 height)
      (apply hc-append
             (map (lambda (bit)
                    (if (= bit 1)
                        (colorize (filled-rectangle width height) "black")
                        (colorize (filled-rectangle width height) "white")))
                  sequence))))

; Рисует последовательность и добавляет вокруг неё рамку
(define (show-sequence sequence [width 20] [height 80])
  (let ([barcode (draw-bars sequence width height)])
    (cc-superimpose
     (colorize (filled-rectangle (+ (pict-width barcode) 10) (+ (pict-height barcode) 10)) "black")
     (colorize (filled-rectangle (+ (pict-width barcode) 8) (+ (pict-height barcode) 8)) "white")
     barcode)))

;1
(define (bar-lines sequence)
  (show-sequence sequence))
(slide (t "Результат функции bar-lines") (t "Вход: (0 0 1 1 0 0 1)") (bar-lines '(0 0 1 1 0 0 1)))

;2
(define (l-code digit)
  (cond
    [(= digit 0) '(0 0 0 1 1 0 1)]
    [(= digit 1) '(0 0 1 1 0 0 1)]
    [(= digit 2) '(0 0 1 0 0 1 1)]
    [(= digit 3) '(0 1 1 1 1 0 1)]
    [(= digit 4) '(0 1 0 0 0 1 1)]
    [(= digit 5) '(0 1 1 0 0 0 1)]
    [(= digit 6) '(0 1 0 1 1 1 1)]
    [(= digit 7) '(0 1 1 1 0 1 1)]
    [(= digit 8) '(0 1 1 0 1 1 1)]
    [(= digit 9) '(0 0 0 1 0 1 1)]
    [else (error "Цифра должна быть от 0 до 9")]))

(slide (t "L-код для цифры 7")
       (t "Вход: (l-code 7)")
       (t "Результат: (0 1 1 1 0 1 1)")
       (bar-lines (l-code 7)))

;3
(define (r-code digit)
  (map (lambda (bit) (if (= bit 0) 1 0)) (l-code digit)))

(slide (t "R-код для цифры 7")
       (t "Вход: (r-code 7)")
       (t "Результат: (1 0 0 0 1 0 0)")
       (bar-lines (r-code 7)))

;4
(define (g-code digit)
  (if (or (< digit 0) (> digit 9))
      (error "Цифра должна быть от 0 до 9")
      (reverse (r-code digit))))

(slide (t "G-код для цифры 7")
       (t "Вход: (g-code 7)")
       (t "Результат: (0 0 1 0 0 0 1)")
       (bar-lines (g-code 7)))

;5
(define (bar-encode-with code-types digits)
  (if (empty? digits)
      '()
      (let ([code-type (car code-types)]
            [digit (car digits)])
        (let ([encoded-digit (cond
                               [(equal? code-type 'L) (l-code digit)]
                               [(equal? code-type 'G) (g-code digit)]
                               [(equal? code-type 'R) (r-code digit)]
                               [else (error "Неизвестный тип кода. Используйте L, G или R")])])
          (append encoded-digit (bar-encode-with (cdr code-types) (cdr digits)))))))

(define encoded-result (bar-encode-with '(L G R) '(1 2 3)))
(define encoded-barcode (bar-lines encoded-result))
(slide (t "Кодирование с разными типами кодов")
       (t "Вход: (bar-encode-with '(L G R) '(1 2 3))")
       (t (format "Результат: ~a" encoded-result))
       encoded-barcode)

;6
(define (bar-code-EAN-8 digits)
  (if (not (= (length digits) 8))
      (error "EAN-8 должен содержать ровно 8 цифр")
      (let* ([guard (draw-bars '(0 1 0 1 0) 2 110)]
             [left-part (draw-bars (bar-encode-with '(L L L L) (take digits 4)) 2 100)]
             [right-part (draw-bars (bar-encode-with '(R R R R) (drop digits 4)) 2 100)]
             [barcode (ht-append guard left-part guard right-part guard)])
        barcode)))

(slide (t "Штрихкод EAN-8 для '(1 2 3 4 5 6 7 8)")
       (t "Левые 4 цифры → L-код, правые 4 → R-код")
       (t "Обрамление: '(0 1 0 1 0) слева, по центру и справа")
       (bar-code-EAN-8 '(1 2 3 4 5 6 7 8)))

;7
(define first-digit-encoding
  '((0 L L L L L L) (1 L L G L G G)
                    (2 L L G G L G)
                    (3 L L G G G L)
                    (4 L G L L G G)
                    (5 L G G L L G)
                    (6 L G G G L L)
                    (7 L G L G L G)
                    (8 L G L G G L)
                    (9 L G G L G L)))

(define (bar-code-EAN-13 digits)
  (if (not (= (length digits) 13))
      (error "EAN-13 должен содержать ровно 13 цифр")
      (let* ([first-digit (car digits)]
             [structure (assoc first-digit first-digit-encoding)]
             [code-types (cdr structure)]
             [guard (draw-bars '(0 1 0 1 0) 2 110)]
             [left-part (draw-bars (bar-encode-with code-types (take (cdr digits) 6)) 2 100)]
             [right-part (draw-bars (bar-encode-with '(R R R R R R) (drop digits 7)) 2 100)])
        (ht-append guard left-part guard right-part guard))))

(slide (t "Штрихкод EAN-13") (bar-code-EAN-13 '(1 2 3 4 5 6 7 8 9 0 1 2 3)))
