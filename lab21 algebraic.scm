;(use-syntax (ice-9 syncase))

(define (define-make% name cases)
  (map (lambda (case)
         (eval `(define ,case
                  (list ',name ',case ,@(cdr case)))
               (interaction-environment))) cases))

(define (type? p cases)
  (and (not (null? cases))
       (or (equal? p (car cases))
           (type?  p (cdr cases)))))

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
                  (eq? (length (cdadr p)) (length (caddr p)))
                  (null? (cdddr p))))
          (interaction-environment))))

(define (define-data% name cases)
  (define-make%  name cases)
  (define-pred%  name cases))

(define-syntax define-data
  (syntax-rules ()
    ((_ name ((case a ...) ...))
     (define-data% (quote name) (quote ((case a ...) ...))))))

(define-syntax match
  (syntax-rules()
    ((_ p) #f)
    ((_ p ((type1 a1 ...) (body1 ...)) ((type2 a2 ...) (body2 ...)) ...)
     (if (equal? (quote (type1 a1 ...)) (cadr p))
         (let ((cp (cdadr p))
               (ap (cddr p)))
           (apply (eval `(lambda ,cp (body1 ...))(interaction-environment)) ap))
         (match p ((type2 a2 ...) (body2 ...)) ...)))))

;; tests

#|(begin
  (define-data figure ((square a)
                       (rectangle a b)
                       (triangle a b c)
                       (circle r)))
  
  (define s (square 10))
  (define r (rectangle 10 20))
  (define t (triangle 10 20 30))
  (define c (circle 10))
  
  (define test-1 (and (figure? s)
                      (figure? r)
                      (figure? t)
                      (figure? c)))
  
  (define a '(circle 0 0 1))
  (define b #f)
  
  (define test-2 (and (figure? a)
                      (figure? b)))
  
  (define pi (acos -1))
  
  (define (perim f)
    (match f 
      ((square a)       (* 4 a))
      ((rectangle a b)  (* 2 (+ a b)))
      ((triangle a b c) (+ a b c))
      ((circle r)       (* 2 pi r))))
  
  (list test-1
        test-2
        (perim s)
        (perim r)
        (perim t)
        (round (perim c)))
  )|#