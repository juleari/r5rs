(define (my-flatten xs)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (if (list? x)
            (append (my-flatten x) (my-flatten (cdr xs)))
            (append (list x) (my-flatten (cdr xs)))))))

(define (get-power p)
  (let ((x (if (list? p)
               (append (list '-) p (list 1))
               (list '- p 1))))
    (eval x (interaction-environment))))

(define (remove-from-list x xs)
  (if (null? xs)
      '()
      (let ((c (car xs))
            (s (remove-from-list x (cdr xs))))
        (if (equal? x c)
            s
            (cons (car xs) s)))))

(define (evaluate expr neutral)
  (cond ((not (list? expr)) expr)
        ((null? expr) neutral)
        ((member 'x (my-flatten (cdr expr)))
         (let* ((op  (car expr))
                (net (if (or (eq? op '*) (eq? op '/))
                         1
                         0))
                (ex (append (list (car expr))
                            (map (lambda (e) (apply evaluate (list e net))) (cdr expr)))))
           (if (member 0 ex)
               (if (or (eq? op '*) (eq? op '/))
                   0
                   (if (eq? op '+)
                       (evaluate (remove-from-list 0 ex) 0)
                       ex))
               (if (member 'x (my-flatten ex))
                   (if (and (eq? op '+) (eq? 2 (length ex)))
                       (cadr ex)
                       ex)
                   (eval ex (interaction-environment))))))
        (else (eval expr (interaction-environment)))))

(define (create-func-name name)
  (string->symbol (string-append "derivative-" (symbol->string name))))

(define (derivative-expt expr)
  (cons '* (cons (caddr expr)
                 (list (list 'expt (cadr expr) (get-power (caddr expr)))))))

(define (derivative-pow expr)
  (cons '* (append (list (list 'expt (cadr expr) (caddr expr)))
                   (list (list 'log (cadr expr))))))

(define (derivative-cos expr)
  (cons '- (list (list 'sin (cadr expr)))))

(define (derivative-sin expr)
  (list 'cos (cadr expr)))

(define (derivative-exp expr)
  (list 'exp (cadr expr)))

(define (derivative-log expr)
  (list '/ 1 (cadr expr)))

(define (derivative expr)
  
  (define (derivative-list es)
    (cond ((null? es)      '())
          ((list? (car es)) (append (list (derivative-simple (car es)))
                                    (derivative-list (cdr es))))
          (else             (append (derivative-simple (list (car es)))
                                    (derivative-list (cdr es))))))
  
  (define (derivative-mul lx x xs)
    (let ((dx (if (list? x)
                  (list (derivative-simple x))
                  (derivative-simple (list x)))))
      (if (null? xs)
          (list (cons '* (append lx dx)))
          (append (list (cons '* (append lx dx xs)))
                  (derivative-mul (append lx (list x)) (car xs) (cdr xs))))))
  
  (define (derivative-div u v)
    (append (list (cons '- (derivative-mul '() u (list v))))
            (list (cons 'expt (list v 2)))))
  
  (define (derivative-func func expr1 expr2)
    (cons '* (append (list (func expr1))
                     (list (derivative-simple expr2)))))
  
  (define (derivative-base expr)
    (let ((func (create-func-name (car expr))))
      (if (list? (cadr expr))
          (eval `(',derivative-func ,func ',expr (cadr ',expr))(interaction-environment))
          (eval `(,func ',expr)(interaction-environment)))))
  
  (define (derivative-simple expr)
    (if (list? expr)
        (let ((e (car expr)))
          (if (number? e)
              '(0)
              (case e
                ('x    '(1))
                ('+    (cons '+ (derivative-list (cdr expr))))
                ('-    (cons '- (derivative-list (cdr expr))))
                ('*    (cons '+ (derivative-mul  '() (cadr expr) (cddr expr))))
                ('/    (cons '/ (derivative-div (cadr expr) (caddr expr))))
                
                ('expt (if (or (and (list? (cadr expr))
                                    (member 'x (my-flatten (cadr expr)))) 
                               (eqv? 'x (cadr expr)))
                           (derivative-base expr)
                           (derivative-base (cons 'pow (cdr expr)))))
                (else  (derivative-base expr)))))
        (if (number? expr)
            0
            1)))
  
  (evaluate (derivative-simple expr) 0))

;; tests
(derivative '2)
(derivative 'x)
(derivative '(- x))
(derivative '(* 1 x))
(derivative '(- (* 1 x)))
(derivative '(expt x 10))
(derivative '(* 2 (expt x 5)))
(derivative '(expt x -2))
(derivative '(cos x))
(derivative '(* 2 (exp x)))
(derivative '(* 2 (exp (* 2 x))))
(derivative '(log x))
(derivative '(* 3 (log x)))
(derivative '(+ (expt x 3) (expt x 2)))
(derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2))))
(derivative '(/ 3 x))
(derivative '(/ 3 (* 2 (expt x 2))))
(derivative '(* 2 (sin x) (cos x)))
(derivative '(* 2 (exp x) (sin x) (cos x)))
(derivative '(sin (* 2 x)))
(derivative '(cos (* 2 (expt x 2))))