;(use-syntax (ice9-syncase))

#|(define (get-kv% kv)
  (define (helper kv k v)
    (if (null? kv)
        (list (reverse k) (reverse v))
        (helper (cdr kv) (cons (caar kv) k) (cons (cadar kv) v))))
  (helper kv '() '()))

(define (my-let% kv body)
  (write (car kv))
  (newline)
  (write (cadr kv))
  (newline)
  (apply (lambda (car kv) (begin body)) (cadr kv)))|#

(define-syntax my-let
  (syntax-rules ()
    ((_ ((k1 v1)) body ...)             ((lambda (k1) body ...) v1))
    ((_ ((k1 v1) (k2 v2) ...) body ...) ((lambda (k1) (my-let ((k2 v2) ...) body ...)) v1))))

(my-let ((a 1)(b (+ 7 2)) (- a b)(+ a b)))

#|(define-syntax my-let*
  (syntax-rules ()
    ((_ ((k1 v1)) body ...)             ((lambda (k1) body ...) v1))
    ((_ ((k1 v1) (k2 v2) ...) body ...) ((lambda (k1) (my-let* ((k2 v2) ...) body ...)) v1))))|#