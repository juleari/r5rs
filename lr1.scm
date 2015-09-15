; 1
(define (count x xs)
  (cond ((null? xs)          0)
        ((equal? x (car xs)) (+ 1 (count x (cdr xs))))
        (else                (count x (cdr xs)))))

;; tests
(count 'a '(a b c a))
(count 'b '(a c d))
(count 'a '())

; 2
(define (replace pred? proc xs)
  (cond ((null? xs)       xs)
        ((pred? (car xs)) (cons (proc (car xs)) (replace pred? proc (cdr xs))))
        (else             (cons       (car xs)  (replace pred? proc (cdr xs))))))

;; tests
(replace zero? (lambda (x) (+ x 1)) '(0 1 2 3 0))
(replace odd?  (lambda (x) (* 2 x)) '(1 2 3 4 5 6))
(replace even? (lambda (x) (/ x 2)) '(1 3 5 7))
(replace (lambda (x) (> 0 x)) exp '())

; 3
(define (replicate x n)
  (if (= 0 n)
      '()
      (cons x (replicate x (- n 1)))))

;; tests
(replicate 'a 5)
(replicate '(a b) 3)
(replicate 'a 0)

; 4
(define (cycle xs n)
  (define (helper n)
    (if (= 0 n)
        '()
        (append xs (helper (- n 1)))))
  
  (helper n))

;; tests
(cycle '(0 1) 3)
(cycle '(a b c) 3)
(cycle '() 0)

; 5
(define and-fold
  (lambda xs
    (define (helper xs)
      (or (null? xs)
          (and (car xs)
               (helper (cdr xs)))))
    (helper xs)))

(define and-fold-cond
  (lambda xs
    (define (helper xs)
      (cond ((null? xs)     #t)
            ((not (car xs)) #f)
            (else           (helper (cdr xs)))))
    (helper xs)))

(define or-fold
  (lambda xs
    (define (helper xs)
      (cond ((null? xs)     #f)
            ((car xs)       #t)
            (else           (helper (cdr xs)))))
    (helper xs)))

;; tests
(and-fold #f #f #f)
(and-fold #t #f #f)
(and-fold #t #t #f)
(and-fold #t #t #t)
(and-fold)

(or-fold #f #f #f)
(or-fold #t #f #f)
(or-fold #t #t #f)
(or-fold #t #t #t)
(or-fold)

; 6
(define o
  (lambda xs
    (lambda (x)
      (define (helper xs)
        (if (null? xs)
            x
            ((car xs) (helper (cdr xs)))))
      (helper xs))))

;; tests
(define (f x) (* x 2))
(define (g x) (* x 3))
(define (h x) (- x))
((o f g h) 1)
((o f g) 1)
((o h) 1)
((o) 1)

; 7
(define (find-number a b c)
  (if (<= a b)
       (if (= 0 (remainder a c))
           a
           (find-number (+ 1 a) b c))
       #f))

;; tests
(find-number 0 5 2)
(find-number 7 9 3)
(find-number 3 7 9)