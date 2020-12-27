;; https://repl.it/@begoon/callcc

(let (
  (continuation (call/cc
    (lambda (c) 
      (display "this should be printed\n")
      (c 100)
      (display "this should NOT be printed\n")))))
  (display (format "continuation=~s\n" continuation))

(display "\n")

(let (
  (continuation (call/cc (lambda (c) c))))
  (if (procedure? continuation)
    (begin 
      (display (format "1. continuation~%"))
      (continuation "continuation is now a string"))
    (begin 
      (display (format "2. NOT continuation\nvalue=~s\n" continuation))
  )
)
