;(use-syntax (ice9-syncase))

(define-syntax my-let
  (syntax-rules ()
    ((_ ((k1 v1)) body ...)                             ((lambda (k1) body ...) v1))
    ((_ (k ...) (v ...) ((k1 v1)) body ...)             ((lambda (k ... k1) body ...) v ... v1))
    ((_ (k ...) (v ...) ((k1 v1)) body ...)             ((lambda (k ... k1) body ...) v ... v1))
    ((_ (k ...) (v ...) ((k1 v1) (k2 v2) ...) body ...)
     (my-let (k ... k1) (v ... v1) ((k2 v2) ...) body ...))
    ((_ ((k1 v1) (k2 v2) ...) body ...)
     (my-let (k1) (v1) ((k2 v2) ...) body ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((_ ((k1 v1)) body ...)             ((lambda (k1) body ...) v1))
    ((_ ((k1 v1) (k2 v2) ...) body ...) ((lambda (k1) (my-let* ((k2 v2) ...) body ...)) v1))))

;; tests
#|(my-let ((a 1)(b 2)(c 3)) (+ a 5)(+ a b c))
(my-let* ((a 1)(b (+ a 2))) (- a b)(+ a b))|#