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
  (let ((barcode (draw-bars sequence width height)))
    (cc-superimpose
     (colorize (filled-rectangle (+ (pict-width barcode) 10) (+ (pict-height barcode) 10)) "black")
     (colorize (filled-rectangle (+ (pict-width barcode) 8) (+ (pict-height barcode) 8)) "white")
     barcode)))

;1
(define (bar-lines sequence)
  (show-sequence sequence))
(define result (bar-lines '(0 0 1 1 0 0 1)))
(slide
 (t "Результат функции bar-lines")
 (t "Вход: (0 0 1 1 0 0 1)")
 result)



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

(define l-code-result (l-code 7))
(define l-code-barcode (bar-lines l-code-result))
(slide
 (t "L-код для цифры 7")
 (t "Вход: (l-code 7)")
 (t "Результат: (0 1 1 1 0 1 1)")
 l-code-barcode)



;3
(define (r-code digit)
  (if (or (< digit 0) (> digit 9))
      (error "Цифра должна быть от 0 до 9")
      (map (lambda (bit) (if (= bit 0) 1 0)) (l-code digit))))

(define r-code-result (r-code 7))
(define r-code-barcode (bar-lines r-code-result))
(slide
 (t "R-код для цифры 7")
 (t "Вход: (r-code 7)")
 (t "Результат: (1 0 0 0 1 0 0)")
 r-code-barcode)



;4
(define (g-code digit)
  (if (or (< digit 0) (> digit 9))
      (error "Цифра должна быть от 0 до 9")
      (reverse (r-code digit))))

(define g-code-result (g-code 7))
(define g-code-barcode (bar-lines g-code-result))

(slide
 (t "G-код для цифры 7")
 (t "Вход: (g-code 7)")
 (t "Результат: (0 0 1 0 0 0 1)")
 g-code-barcode)



;5
(define (bar-encode-with code-types digits)
  (if (empty? digits)
      '()
      (let ((code-type (car code-types))
            (digit (car digits)))
        (let ((encoded-digit (cond
                               [(equal? code-type 'L) (l-code digit)]
                               [(equal? code-type 'G) (g-code digit)]
                               [(equal? code-type 'R) (r-code digit)]
                               [else (error "Неизвестный тип кода. Используйте L, G или R")])))
          (append encoded-digit 
                  (bar-encode-with (cdr code-types) (cdr digits)))))))

(define encoded-result (bar-encode-with '(L G R) '(1 2 3)))
(define encoded-barcode (bar-lines encoded-result))

(slide
 (t "Кодирование с разными типами кодов")
 (t "Вход: (bar-encode-with '(L G R) '(1 2 3))")
 (t (format "Результат: ~a" encoded-result)) 
 encoded-barcode)



 ;6
(define (bar-code-EAN-8 digits)
  (if (not (= (length digits) 8))
      (error "EAN-8 должен содержать ровно 8 цифр")
      (let* ((left-digits (take digits 4))
             (right-digits (drop digits 4))
             (left-encoded (apply append (map l-code left-digits)))
             (right-encoded (apply append (map r-code right-digits)))
             (guard-pattern '(0 1 0 1 0))

             (guard (draw-bars guard-pattern 2 110))

             (left-part (draw-bars left-encoded 2 100))
             (right-part (draw-bars right-encoded 2 100))

             (barcode (ht-append
                           guard 
                           left-part
                           guard
                           right-part
                           guard)))
              barcode)))

(define ean8-result (bar-code-EAN-8 '(1 2 3 4 5 6 7 8)))

(slide
 (t "Штрихкод EAN-8 для '(1 2 3 4 5 6 7 8)")
 (t "Левые 4 цифры → L-код, правые 4 → R-код")
 (t "Обрамление: '(0 1 0 1 0) слева, по центру и справа")
 ean8-result)



;7
