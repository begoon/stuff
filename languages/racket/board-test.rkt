#lang racket/base
 
(require rackunit "board.rkt")
(require racket/file)
(require racket/list)

(test-case
 "make-board"
 (check-exn
  (regexp "alphabet length must be square")
  (lambda () (make-board "12")))
 (check-equal? (make-board "abcdefghi") '(0 1 2 3 4 5 6 7 8))
 (check-equal? (make-board alphabet) (range 100)))

(test-case
 "board-width"
 (check-equal? (board-width (make-board "123456789")) 3))

(test-case
 "sqrt?"
 (check-true (sqrt? 9))
 (check-false (sqrt? 8)))

(test-case
 "drop-n"
 (check-equal? (drop-n '() 0) '())
 (check-equal? (drop-n '() 1) '())
 (check-equal? (drop-n '(1 2 3) 2) '(3))
 (check-equal? (drop-n '(1 2) 10) '()))

(test-case
 "split-by"
 (check-equal? (split-by '() 0) '())
 (check-equal? (split-by '(1) 0) '())
 (check-equal? (split-by '(0) 1) '((0)))
 (check-equal? (split-by '(1 2) 2) '((1 2)))
 (check-equal? (split-by '(1 2) 3) '((1 2)))
 (check-equal? (split-by '(1 2) 1) '((1) (2)))
 (check-equal? (split-by '(1 2 3 4 5) 2) '((1 2) (3 4) (5)))
 )

(check-equal?
 (make-columns board)
 '((9 19 29 39 49 59 69 79 89 99)
   (8 18 28 38 48 58 68 78 88 98)
   (7 17 27 37 47 57 67 77 87 97)
   (6 16 26 36 46 56 66 76 86 96)
   (5 15 25 35 45 55 65 75 85 95)
   (4 14 24 34 44 54 64 74 84 94)
   (3 13 23 33 43 53 63 73 83 93)
   (2 12 22 32 42 52 62 72 82 92)
   (1 11 21 31 41 51 61 71 81 91)
   (0 10 20 30 40 50 60 70 80 90)))

(check-equal?
 (make-rows board)
 '((0 1 2 3 4 5 6 7 8 9)
   (10 11 12 13 14 15 16 17 18 19)
   (20 21 22 23 24 25 26 27 28 29)
   (30 31 32 33 34 35 36 37 38 39)
   (40 41 42 43 44 45 46 47 48 49)
   (50 51 52 53 54 55 56 57 58 59)
   (60 61 62 63 64 65 66 67 68 69)
   (70 71 72 73 74 75 76 77 78 79)
   (80 81 82 83 84 85 86 87 88 89)
   (90 91 92 93 94 95 96 97 98 99)))

(check-equal?
 (format-board board)
 '(" 0  1  2  3  4  5  6  7  8  9"
   "10 11 12 13 14 15 16 17 18 19"
   "20 21 22 23 24 25 26 27 28 29"
   "30 31 32 33 34 35 36 37 38 39"
   "40 41 42 43 44 45 46 47 48 49"
   "50 51 52 53 54 55 56 57 58 59"
   "60 61 62 63 64 65 66 67 68 69"
   "70 71 72 73 74 75 76 77 78 79"
   "80 81 82 83 84 85 86 87 88 89"
   "90 91 92 93 94 95 96 97 98 99"))

(test-case
 "print-board"
 (let ([temp-file (make-temporary-file "test-~a" #f ".")])
   (around
    (with-output-to-file temp-file #:exists 'truncate
      (lambda () (print-board board)))
    (with-input-from-file temp-file
      (lambda ()
        (check-equal?
         (file->lines temp-file)
         '(" 0  1  2  3  4  5  6  7  8  9"
           "10 11 12 13 14 15 16 17 18 19"
           "20 21 22 23 24 25 26 27 28 29"
           "30 31 32 33 34 35 36 37 38 39"
           "40 41 42 43 44 45 46 47 48 49"
           "50 51 52 53 54 55 56 57 58 59"
           "60 61 62 63 64 65 66 67 68 69"
           "70 71 72 73 74 75 76 77 78 79"
           "80 81 82 83 84 85 86 87 88 89"
           "90 91 92 93 94 95 96 97 98 99"))))
    (delete-file temp-file))))

(check-equal?
 (format-lines '((0 1) (2 3 4))) '(" 0  1" " 2  3  4"))

(test-case
 "print-lines"
 (let ([temp-file (make-temporary-file "test-~a" #f ".")])
   (around
    (with-output-to-file temp-file #:exists 'truncate
      (lambda () (print-lines '((0) (1 2) (3 4 5)))))
    (with-input-from-file temp-file
      (lambda ()
        (check-equal?
         (file->lines temp-file)
         '(" 0" " 1  2" " 3  4  5"))))
    (delete-file temp-file))))

