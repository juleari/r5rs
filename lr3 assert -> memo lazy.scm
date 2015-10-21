;lab3

;#1
(define g-e #f)

(define (use-assertions)
  (call-with-current-continuation
   (lambda (e)
     (set! g-e e))))

(define (assert% cond-value cond-expr)
  (if (not cond-value)
      (begin
        (newline)
        (display "FAILED ")
        (write cond-expr)
        (newline)
        (g-e))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr) (assert% expr 'expr))))

(use-assertions)
(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(-2 -1 0 1 2))
(write (map 1/x '(1 2 3 4 5)))

;#2
(define-syntax &
  (syntax-rules (->)
    ((_ -> body ...)     (lambda () (begin body ...)))
    ((_ x -> body ...)   (lambda (x) (begin body ...)))
    ((_ x y -> body ...) (lambda (x y) (begin body ...)))))

(define-syntax &
  (syntax-rules ()
    ((_ exprs ...) (let loop ((args '())
                              (body '(exprs ...)))
                     (if (equal? (car body) '->)
                         (eval `(lambda ,args . ,(cdr body))(interaction-environment))
                         (loop (append args (list (car body))) (cdr body)))))))

((& -> (display 1)))
((& x y -> (display (+ x y))(newline)(display (- x y))) 3 2)
((& x y z -> (display (+ x y z))(newline)(display (- x y))(newline)(display (- x z))(newline)) 5 7 1)

;#3
(define memo-A
  (let ((a-table '()))
    (lambda (m n)
      (let ((Amn (assoc (list m n) a-table)))
        (or (and Amn (cadr Amn))
            (and (zero? m)
                 (let ((A0n (+ 1 n)))
                   (set! a-table (cons (list (list m n) A0n) a-table))
                   A0n))
            (and (zero? n)
                 (let ((Am0 (memo-A (- m 1) 1)))
                   (set! a-table (cons (list (list m n) Am0) a-table))
                   Am0))
            (let ((A-mn (memo-A (- m 1) (memo-A m (- n 1)))))
              (set! a-table (cons (list (list m n) A-mn) a-table))
              A-mn))))))

(define (A m n)
  (cond ((zero? m) (+ n 1))
        ((zero? n) (A (- m 1) 1))
        (else      (A (- m 1) (A m (- n 1))))))

(memo-A 3 7)

;#4
(define-syntax lazy-expr
  (syntax-rules ()
    ((_ expr) (delay expr))))

(define-syntax my-if
  (syntax-rules ()
    ((_ cond? true false)
     (or (and cond? (force (lazy-expr true)))
         (force (lazy-expr false))))))

(my-if #t 1 (/ 1 0))
(my-if #f (/ 1 0) 1)