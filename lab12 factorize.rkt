(define (factorize expr)
  (define (expr-expt? expr pow)
    (and (list? expr)
         (= (length expr) 3)
         (eqv? (car expr) 'expt)
         (or (symbol? (cadr expr)) (list? (cadr expr)) (number? (cadr expr)))
         (= (caddr expr) pow)))
  
  (define (factorize-sub-cub a b)
    (list '*
          (list '- a b)
          (list '+
                (list 'expt a 2)
                (list '* a b)
                (list 'expt b 2))))
  
  (define (factorize-add-cub a b)
    (list '*
          (list '+ a b)
          (list '+
                (list 'expt a 2)
                (cons '- (list (list '* a b)))
                (list 'expt b 2))))
  
  (define (factorize-sub-sqr a b)
    (list '*
          (list '- a b)
          (list '+ a b)))
  
  (let ((expt-a (cadr expr))
        (expt-b (caddr expr)))
    (cond ((and (eqv? (car expr) '-) (expr-expt? expt-a 3) (expr-expt? expt-b 3)) (factorize-sub-cub (cadr expt-a) (cadr expt-b)))
          ((and (eqv? (car expr) '+) (expr-expt? expt-a 3) (expr-expt? expt-b 3)) (factorize-add-cub (cadr expt-a) (cadr expt-b)))
          ((and (eqv? (car expr) '-) (expr-expt? expt-a 2) (expr-expt? expt-b 2)) (factorize-sub-sqr (cadr expt-a) (cadr expt-b))))))

;; tests
(define expr '(- (expt (- a dc) 3) (expt b 3)))
(factorize expr)
(factorize '(- (expt x 2) (expt y 2)))
  
(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
             
(eval (list (list 'lambda 
                          '(x y) 
                          (factorize '(- (expt x 2) (expt y 2))))
                    1 2)
              (interaction-environment))