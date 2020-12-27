;; https://repl.it/@begoon/callcc

(let (
  (value (call/cc
    (lambda (the-continuation) 
      (display "this should be printed\n")
      (the-continuation 100)
      (display "this should NOT be printed\n")))))
  (display (format "value=~s\n" value))

(display "\n")

(let (
  (value (call/cc
      (lambda (the-continuation) 
        the-continuation))))
  (if (procedure? value)
    (begin 
      (display (format "1. continuation~%"))
      (value "value is now a string"))
    (begin 
      (display (format "2. NOT continuation\nvalue=~s\n" value))
  )
)
