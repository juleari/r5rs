;gail
;(define-macro (tut a b) `(+ a b))

;hygienic macros
(define-syntax test
  (syntax-rules ()
    ;((test a b) (+ a b))))
    ((_ a b) (+ a b))))

;(test 1 2)

(define-syntax swap!
  (syntax-rules ()
    ((_ a b) (let ((c a))
               (set! a b)
               (set! b c)))))

;(define x 1)
;(define y 2)
;(swap! x y)
;(list  x y)

;when unless

(define-syntax when
  (syntax-rules ()
    ((_ cond_expr expr . exprs)
     (if cond_expr (begin expr . exprs)))))

(define-syntax for
  (syntax-rules (in as)
    ((_ item in items body)
     (for-each (lambda (item) body) items))
    ((_ items as item body)
     (for item in items body))))

(define-syntax reverse-order
  (syntax-rules ()
    ((_ (e . rest)) (append (reverse-order rest) (list e)))
    ((_ e)          (list e))))

(reverse-order (1 2 3 4 +))