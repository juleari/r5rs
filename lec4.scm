(define x (delay (+ 1 2)))

(force x)

(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (lazy-cdr ls) (force (cdr ls)))
(define lazy-car car)

(define (fib-gen a b)
  (lazy-cons a (fib-gen b (+ a b))))

(define (lazy-ref ls n)
  (if (zero? n)
      (lazy-car ls)
      (lazy-ref (lazy-cdr ls) (- n 1))))

#|
input-port?
output-port?

(current-input-port)
(close-input-port port)
(close-output-port port)
(open-input-file path)
(open-output-file path)

(read)
(read port)
(read-char [port])
(peek-char [port])
(eof-object?)
(char-ready? port)

(write x port)
(display x port)
(newline)

with-input-to-file
with-output-to-file
call-with-input-file
call-with-output-file
|#

(define name1 'abc)
(define name2 'x)
`(,name1 ,name2)