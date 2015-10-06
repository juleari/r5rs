;(use-syntax (ice-9 syncase))
(define-syntax when
  (syntax-rules ()
    ((_ cond_expr expr ...)
     (if cond_expr (begin expr ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond_expr expr ...)
     (when (not cond_expr) expr ...))))

(define-syntax for
  (syntax-rules (in as)
    ((_ item in items proc ...)
     (for-each (lambda (item) (begin proc ...)) items))
    ((_ items as item . procs) (for item in items . procs))))

(define-syntax while
  (syntax-rules ()
    ((_ cond? proc ...)
     (letrec ((iter (lambda () (if cond?
                                   (begin (begin proc ...)
                                          (iter))))))
       (iter)))))

(define-syntax repeat
  (syntax-rules (until)
    ((_ procs until cond?)
     (begin
       (begin . procs)
       (while (not cond?) . procs)))))

(define-syntax cout
  (syntax-rules (<< endl)
    ((_ << endl)       (newline))
    ((_ << endl x ...) (begin (newline)      (cout x ...)))
    ((_ << elem x ...) (begin (display elem) (cout x ...)))))


#|;; tests
(define x 1)
(when   (> x 0) (display "x > 0")  (newline))
(unless (= x 0) (display "x != 0") (newline))

(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
    (for '(4 5 6) as j
      (display (list i j))
      (newline)))

(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))

(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))

(cout << "a = " << 1 << endl << "b = " << 2 << endl)
|#