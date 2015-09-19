(string->number "3")
(symbol? 'a)
#\newline
#\null
#\space

;(pair? x)

(list? '(1 2 3))
(vector? #(1 2 3))
(string=? "121" "121")

(define a 1)
(define b 2)

(eval '(+ a b) (interaction-environment))

(define expr '(- (expt (- a dc) 3) (expt b 3)))
(* (- a b) (+ (expt a 2) (* a b) (expt b 2)))

;symbol-concat

(define (factorize expr)
  (define (expr? expr)
    )
  (define (expr-expt? expr)
    (and (list? expr)
         (eqv? (car expr) 'expt)
         (or (symbol? (cadr expr)) (list? (cadr expr)))
         (= (caddr expr) 3)))
  (if (eqv? (car expr) '-)
      (let ((expt-a (cadr expr))
            (expt-b (caddr expr)))
        (if (expr-expt? expt-a)
            (let ((a (cadr expt-a)))
              (if (expr-expt? expt-b)
                  (let ((b (cadr expt-b)))
                    (list '*
                          (list '- a b)
                          (list '+
                                (list 'expt a 2)
                                (list '* a b)
                                (list 'expt b 2))))))))))

(factorize expr)