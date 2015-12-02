;(use-syntax (ice-9 syncase))

(define (or-fold . xs)
  (and (not (null? xs))
       (or (car xs) (apply or-fold (cdr xs)))))

(define-syntax define-data
  (syntax-rules ()
    ((_ name ((dataname arg ...) ...))
     (begin (map (lambda (types)
                   (eval `(define ,types 
                            (list 'name ',(car types) ,@(cdr types)))
                         (interaction-environment)))
                 '((dataname arg ...) ...))
            (eval `(define (,(string->symbol 
                              (string-append (symbol->string 'name) "?")) x)
                     (and (list? x)
                          (eq? 'name (car x))
                          (let ((y (cdr x)))
                            (apply or-fold 
                                   ,`(map (lambda (types)
                                            (and (eq? (car types) (car ,'y))
                                                 (eq? (length types) 
                                                      (length ,'y))))
                                          '((dataname arg ...) ...))))))
                  (interaction-environment))))))

(define-syntax match
  (syntax-rules()
    ((_ p) #f)
    ((_ p ((type1 a1 ...) (body1 ...)) ((type2 a2 ...) (body2 ...)) ...)
     (if (and (equal? (quote type1) (cadr p))
              (equal? (length (quote (type1 a1 ...))) (length (cdr p))))
         (apply (eval `(lambda (a1 ...) (body1 ...))(interaction-environment)) 
                (cddr p))
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