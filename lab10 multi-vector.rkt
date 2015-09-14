(define (my-fold-right op xs)
  (if (= (length xs) 2)
      (op (car xs) (cadr xs))
      (op (car xs) (my-fold-right op (cdr xs)))))

(define (make-vector-size sizes fill)
  (define (helper xs size)
    (if (= (length xs) size)
        (list->vector (cons 'multi-vector (cons (length sizes) (append sizes xs))))
        (helper (cons fill xs) size)))
  
  (helper '() (my-fold-right * sizes)))

(define-syntax make-multi-vector
  (syntax-rules ()
    ((_ sizes)      (make-vector-size sizes 0))
    ((_ sizes fill) (make-vector-size sizes fill))))

(define (multi-vector? m)
  (define (take-mul num xs)
    (if (= 0 num)
        1
        (* (car xs) (take-mul (- num 1) (cdr xs)))))
  
  (and (vector? m)
       (> (vector-length m) 2)
       (eqv? (vector-ref m 0) 'multi-vector)
       (> (vector-length m) (+ 2 (vector-ref m 1)))
       (= (vector-length m) (+ 2 (vector-ref m 1) (take-mul (vector-ref m 1) (cddr (vector->list m)))))))

(define (take-lens m)
  (define (helper xs ms)
    (if (= (length xs) (vector-ref m 1))
        xs
        (helper (append xs (list (car ms))) (cdr ms))))

  (helper '() (cddr (vector->list m))))

(define (get-index indices xs)
  (define (helper indx xs)
    (if (null? xs)
        (+ 2 (length indices) (car indx))
        (+ (my-fold-right * (cons (car indx) xs)) (helper (cdr indx) (cdr xs)))))

  (helper indices xs))

(define (multi-vector-ref m indices)
  (and (multi-vector? m)
       (= (length indices) (vector-ref m 1))
       (vector-ref m (get-index indices (cdr (take-lens m))))))

(define (multi-vector-set! m indices x)
  (and (multi-vector? m)
       (= (length indices) (vector-ref m 1))
       (vector-set! m (get-index indices (cdr (take-lens m))) x)))

;; tests
(define m (make-multi-vector '(11 12 9 16)))
(multi-vector? m)
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12))

(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0))

(define m (make-multi-vector '(2 4 2)))
(multi-vector-set! m '(0 0 0) 0)
(multi-vector-set! m '(0 0 1) 1)
(multi-vector-set! m '(0 1 0) 2)
(multi-vector-set! m '(0 1 1) 3)
(multi-vector-set! m '(0 2 0) 4)
(multi-vector-set! m '(0 2 1) 5)
(multi-vector-set! m '(0 3 0) 6)
(multi-vector-set! m '(0 3 1) 7)
(multi-vector-set! m '(1 0 0) 8)
(multi-vector-set! m '(1 0 1) 9)
(multi-vector-set! m '(1 1 0) 10)
(multi-vector-set! m '(1 1 1) 11)
(multi-vector-set! m '(1 2 0) 12)
(multi-vector-set! m '(1 2 1) 13)
(multi-vector-set! m '(1 3 0) 14)
(multi-vector-set! m '(1 3 1) 15)
m