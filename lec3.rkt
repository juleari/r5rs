(define-syntax compose
  (syntax-rules ()
    ((_ () () expr) expr)
    ((_ () (f . rfs) (lambda (x) expr))
     (compose () rfs (lambda (x) (f expr))))
    ((_ (f . fs) rfs expr) (compose fs (f . rfs) expr))
    ((_ f . fs) (compose (f . fs) () (lambda (x) x)))))

((compose sqrt abs sin) -1)

(define-syntax arrows
  (syntax-rules ()
    ((_ q () let-expr) (let* let-expr q))
    ((_ q (var . vars) let-expr)
     (arrows q vars (var . let-expr)))
    ((_ (return q) vars) (arrows q vars ()))
    ((_ (x >- f -> y . rest) vars)
     (arrows rest ((y (f x)) . vars)))
    ((_ arg . args) (arrows (arg . args) ()))))

(define (circuit a b c)
  (arrows
   (list a c)   >- and -> qa
   c            >- not -> c-
   (list c- b)  >- and -> qb
   (list qa qb) >- or  -> q
   return q))

(circuit #f #f #f)

(define (map-1/x xs)
  (call-with-current-continuation
   (lambda (escape)
     (map (lambda (x)
            (if (and (number? x)
                     (not (zero? x)))
                (/ 1 x)
                (escape 'error)))
          xs))))

(map-1/x '(1 2 3 4))
(map-1/x '(1 0 2 a 3))