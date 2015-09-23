;(use-syntax (ice-9 syncase))
#|(define-syntax when
  (syntax-rules ()
    ((_ cond_expr expr . exprs)
     (if cond_expr (begin expr . exprs)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond_expr expr . exprs)
     (when (not cond_expr) expr . exprs))))|#

(define-syntax for
  (syntax-rules (in as)
    ((_ item in items . body)
     (for-each (for item . body) items))
    ((_ items as item . body)
     (for item in items body))
    ((_ item . body) (lambda (item) body))))

;; tests
;(define x 1)
;(when   (> x 0) (display "x > 0")  (newline))
;(unless (= x 0) (display "x != 0") (newline))

(for i in '(1 2 3)
  (display i)
  (newline))

;(for '(1 2 3) as i
;  (for '(4 5 6) as j
;    (display (list i j))
;    (newline)))