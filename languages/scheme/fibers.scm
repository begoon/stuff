;; https://www.youtube.com/watch?v=Ju3KKu_mthg

(define thread-list `())

(define thread-yield
  (lambda ()
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
            (next-thread 'go)
          )
          #t ) ) ) ) )

(define thread-end
  (lambda ()
    (if (null? thread-list)
      (display "exit\n")
      (let* (
          (next-thread (car thread-list))
          (other-threads (cdr thread-list)) 
        )
        (set! thread-list other-threads)
        (next-thread 'go) ) ) ) )

(define thread-new
  (lambda (thread-proc)
    (let (
        (continuation (call/cc (lambda (c) c) ) ) )
      (if (procedure? continuation)
        (begin 
          (set! thread-list (
            append thread-list (list continuation)) )
          (thread-proc)
          (thread-end)
        )
        #t ) ) ) )

(define t1-proc
  (lambda ()
    (display "I'm am 1\n")
    (thread-yield)
    (display "I'm an 1 again\n") ) )
  
(define t2-proc
  (lambda ()
    (display "I'm am 2\n")
    (thread-yield)
    (display "I'm an 2 again\n") ) )

(define t3-proc
  (lambda ()
    (display "I'm am 3\n")
    (thread-yield)
    (display "I'm an 3 again\n") ) )

(thread-new t1-proc)
(thread-new t2-proc)
(thread-new t3-proc)

(thread-end)
