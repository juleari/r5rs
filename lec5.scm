(define g-e #f)

(define (use-assertions)
  (call-with-current-continuation
   (lambda (e)
     (set! g-e e))))

(define (assert% cond-value cond-expr)
  (if (not cond-value)
      (begin
        (newline)
        (display "FAILED")
        (write ))))