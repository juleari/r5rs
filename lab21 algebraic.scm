(use-syntax (ice-9 syncase))

(define (define-make% name cases)
  (map (lambda (case)
         (let ((cname (car case))
               (cargs (cdr case)))
           (eval `(define (,cname . cvals)
                    (list ',name
                          ',case
                          cvals))
                 (interaction-environment)))) cases))


(define (type? p cases)
  (and (not (null? cases))
       (or (equal? p (car cases))
           (type?  p (cdr cases)))))

(define (len xs)
  (if (null? xs)
      0
      (+ 1 (len (cdr xs)))))

(define (define-pred% name cases)
  (let ((pname (string->symbol
                (string-append (symbol->string name)
                               "?"))))
    (eval `(define (,pname p)
             (and (list?  p)
                  (not (null? p))
                  (equal? (car p) ',name)
                  (not (null? (cdr p)))
                  (list? (cadr p))
                  (type? (cadr p) ',cases)
                  (not (null? (cddr p)))
                  (list? (caddr p))
                  (eq? (len (cdadr p)) (len (caddr p)))
                  (null? (cdddr p))))
          (interaction-environment))))

(define (define-match% name cases)
  (eval `(define-syntax match
           (syntax-rules ()
             ((_ p) (write p))
             ((_ p ((type1 a1 ...) (body1 ...)) ((type2 a2 ...) (body2 ...)) ...)
              (begin
                (if (equal? (quote (type1 a1 ...)) (cadr p))
                    (let ((cp (cdadr p))
                          (ap (caddr p)))
                      (apply (eval `(lambda ,cp (body1 ...))(interaction-environment)) ap))
                    (match p ((type2 a2 ...) (body2 ...)) ...))))))
        (interaction-environment)))

(define (define-match% name cases)
  (eval `(define (match p . conds)
           ((if (equal? (cadr p) (caar conds))
                    (let ((cp (cdadr p))
                          (ap (caddr p)))
                      (apply (eval `(lambda ,cp . (cdar conds))(interaction-environment)) ap))
                    (apply match (cons p (cdr conds))))))
        (interaction-environment)))

(define (define-data% name cases)
  (define-make%  name cases)
  (define-pred%  name cases)
  (define-match% name cases))

(define-syntax define-data
  (syntax-rules ()
    ((_ name ((case a ...) ...))
     (define-data% (quote name) (quote ((case a ...) ...))))))

;; tests
#|(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

(and (figure? s)
     (figure? r)
     (figure? t)
     (figure? c))

(define pi (acos -1)) ; Для окружности

(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))

(perim s)
(perim r)
(perim t)|#