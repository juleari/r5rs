;(use-syntax (ice-9 syncase))

(define memoized-factorial
  (let ((n!s '()))
    (lambda (n)
      (let ((m! (assq n n!s)))
        (or (and m! (cadr m!))
            (and (zero? n) 1)
            (let ((n! (* n (memoized-factorial (- n 1)))))
              (set! n!s (cons (list n n!) n!s))
              n!))))))

(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define lazy-car car)
(define (lazy-cdr p) (force (cdr p)))

(define (lazy-head xs k)
  (if (zero? k)
      '()
      (cons (lazy-car xs)
            (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs k)
  (car (reverse (lazy-head xs k))))

(define (naturals start)
  (lazy-cons start (naturals (+ 1 start))))

(define (lazy-factorial n)
  (define (n! x y)
    (let ((x! (* y x)))
      (lazy-cons x! (n! (+ x 1) x!)))
    (lazy-ref (n! 1 1) n)))

;; tests
(begin
  (display (memoized-factorial 10)) (newline)
  (display (memoized-factorial 50)) (newline))

(display (lazy-head (naturals 10) 12))
(display (lazy-ref  (naturals 20) 12))

(begin
  (display (lazy-factorial 10)) (newline)
  (display (lazy-factorial 50)) (newline))
