(define (calc-length sizes)
  (+ 1 (apply * sizes)))

(define (make-multi-vector . args)
  (write args)
  (let* ((sizes (car args))
         (vect  (apply make-vector
                       (cons (calc-length sizes) (cdr args)))))
    (vector-set! vect 0 sizes)
    vect))

(define (multi-vector? m)
  (and (vector? m)
       (list? (vector-ref m 0))
       (eq? (vector-length m) (calc-length (vector-ref m 0)))))

(define (get-index sizes indices)
  (if (null? sizes)
      (+ 1 (car indices))
      (+ (apply * (cons (car indices) sizes))
         (get-index (cdr sizes) (cdr indices)))))

(define (multi-vector-ref m indices)
  (vector-ref m (get-index (cdr (vector-ref m 0)) indices)))

(define (multi-vector-set! m indices x)
  (vector-set! m (get-index (cdr (vector-ref m 0)) indices) x))

;; tests
(define m (make-multi-vector '(11 11 11 11 11) 'x))
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
(begin (define m (make-multi-vector (quote (11 12 9 16)))) (multi-vector? m))