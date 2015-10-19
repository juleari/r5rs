;(use-syntax (ice-9 syncase))

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

(define (nat start)
  (lazy-cons start (nat (+ 1 start))))

(define naturals
  (nat 0))

(define (lazy-map proc . xs)
  (lazy-cons (apply proc (map lazy-car xs))
             (apply lazy-map (cons proc (map lazy-cdr xs)))))

(define (lazy-filter pred? xs)
  (let ((x (lazy-car xs)))
    (if (pred? x)
        (lazy-cons x (lazy-filter pred? (lazy-cdr xs)))
        (lazy-filter pred? (lazy-cdr xs)))))

#| ;; tests
(lazy-head naturals 10)
(lazy-head (lazy-map (lambda (x) (- x)) naturals) 10)
(lazy-head (lazy-map * naturals naturals) 10)
(lazy-head (lazy-filter (lambda (x) (zero? (remainder x 3))) naturals) 10)
(lazy-head (lazy-filter even? naturals) 10)
;|#