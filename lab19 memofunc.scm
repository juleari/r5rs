;(use-syntax (ice-9 syncase))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ (func a ...) body ...) (define func
                                 (let ((memo '()))
                                   (lambda (a ...)
                                     (let ((m (assoc (list a ...) memo)))
                                       (if m
                                           (cadr m)
                                           (let ((res (begin body ...)))
                                             (set! memo
                                                   (cons (list (list a ...) res)
                                                         memo))
                                             res)))))))
    ((_ func (lambda (a ...) body ...)) (define-memoized (func a ...) body ...))))

;; tests
#|(define-memoized (trib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((= n 2) 1)
    (else (+ (trib (- n 1))
             (trib (- n 2))
             (trib (- n 3))))))

(trib 10)

(define-memoized (A m n)
  (cond ((zero? m) (+ n 1))
        ((zero? n) (A (- m 1) 1))
        (else      (A (- m 1) (A m (- n 1))))))
(A 3 7)

(begin
  
  (define-memoized ackermann
    (lambda (m n)
      (cond
        ((= m 0)               (+ n 1))
        ((and (> m 0) (= n 0)) (ackermann (- m 1) 1))
        (else                  (ackermann (- m 1) (ackermann m (- n 1)))))))
  
  (map ackermann '(2 3 3 3) '(3 4 5 6))
  (map ackermann '(2 3 3 3) '(3 4 5 6)))|#