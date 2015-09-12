;gail
;(define-macro (tut a b) `(+ a b))

;hygienic macros
(define-syntax test
  (syntax-rules ()
   ;((test a b) (+ a b))))
    ((_ a b) (+ a b))))

(test 1 2)

(define-syntax swap!
  (syntax-rules ()
    ((_ a b) (let ((c a))
               (set! a b)
               (set! b c)))))

(define x 1)
(define y 2)
(swap! x y)
(list  x y)

;when unless