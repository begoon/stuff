#lang racket

(require racket/file)

(define alphabet
  (string-join
   '("0123456789"
     "abcdefghig"
     "klmnopqrst"
     "uvwxyzABCD"
     "EFGHIGKLMN"
     "OPQRSTVUWX"
     "YZ!@£$%^&*"
     "()_+¡€#¢∞§"
     "¶•ao–≠~`{“"
     "}:|<≤>≥?÷æ")
   ""))

(define (make-board alphabet)
  (define board-length (string-length alphabet))
  (define-values (_ r) (integer-sqrt/remainder board-length))
  (if (= r 0)
      (range board-length)
      (raise-arguments-error
       'make-board
       "alphabet length must be square"
       "alphabet" board
       "length" board-length)))

(define (sqrt? x) (= (sqr (integer-sqrt x)) x))

(define (board-width board) (integer-sqrt (length board)))

(define (split-by xs n)
  (if (and (not (empty? xs)) (> n 0))
      (cons
       (if (> n (length xs)) xs (take xs n))
       (split-by
        (if (> n (length xs)) '() (drop xs n))
        n))
      '()))

(define board (make-board alphabet))

(define (make-rows board)
  (split-by board (board-width board)))

(define (drop-n xs n)
  (if (<= n (length xs))
      (drop xs n)
      '()))

(define (every-nth n xs)
  (if (empty? xs)
      '()
      (cons (first xs)
            (every-nth n (drop-n xs n)))))

(define (make-columns board)
  (define (loop b n result)
    (if (> n 0)
        (loop
         (drop b 1)
         (- n 1)
         (cons (every-nth (board-width board) b) result))
        result))
  (loop board (board-width board) '()))

(define (format-line line)
  (if (not (empty? line))
      (begin
        (cons
         (~r (first line) #:min-width 2 #:pad-string " ")
         (format-line (rest line))))
      '()))

(define (format-board board)
  (map
   (lambda (x)
     (string-join (format-line x)))
   (split-by board (board-width board))))

(define (print-board board)
  (map displayln (format-board board)))

(define (format-lines xs)
  (map
   (lambda (x) (string-join (format-line x)))
   xs))

(define (print-lines lines)
  (map displayln (format-lines lines)))

(provide (all-defined-out))

(print-board board)
;(format-lines (make-rows board))
(print-lines (make-columns board))
