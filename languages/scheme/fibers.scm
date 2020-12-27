;; https://www.youtube.com/watch?v=Ju3KKu_mthg

(define logging #t)

(define debug 
  (lambda (msg) 
    (if logging (display msg) '()) ) )

(define thread-list `())

(define thread-yield
  (lambda ()
    (begin
      (debug "- yield\n")
      (if (null? thread-list)
        #t
        (let (
            (continuation (call/cc (lambda (c) c))) )
          (if (procedure? continuation)
            (let* (
                (next-thread (car thread-list))
                (other-threads (cdr thread-list)) 
                (new-thread-list
                  (append other-threads (list continuation)) )
              )
              (set! thread-list new-thread-list)
              (debug "- switch to another fiber (yield)\n")
              (next-thread 'go)
            )
            (begin (debug "continuation (yield) is called\n") #t) ) ) ) ) ) )

(define thread-end
  (lambda ()
    (if (null? thread-list)
      (debug "exit\n")
      (let* (
          (next-thread (car thread-list))
          (other-threads (cdr thread-list)) 
        )
        (set! thread-list other-threads)
        (debug "- switch to another fiber (end)\n")
        (next-thread 'go) ) ) ) )

(define thread-new
  (lambda (thread-proc)
    (let (
        (continuation (call/cc (lambda (c) 
          (begin 
            (debug "- continuation is created\n") c) ) ) ) )
      (if (procedure? continuation)
        (begin 
          (debug "- continuation is added to threads\n")
          (set! thread-list (
            append thread-list (list continuation)) )
          (thread-proc)
          (thread-end)
        )
        (begin (debug "continuation (new) is called\n") #t) ) ) ) )

(define worker
  (lambda (n) 
    (display(format "I'm am ~s\n" n))
    (thread-yield)
    (display(format "I'm an ~s again\n" n)) 
    (thread-yield) ) )

(define launch 
  (lambda (n)
    (if (= 0 n) 
      ()
      (begin         
        (debug (format "- creating worker ~s\n" n))
        (thread-new (lambda () (worker n) ))
        (launch (- n 1))
      )
    ) 
  )
)

(launch 5)

(thread-end)
