;lab 1
(define (day-of-week day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (- (+ month (* a 12)) 2)))
    (remainder (- (+ 7000 day y (quotient y 4) (quotient y 400) (quotient (* 31 m) 12)) (quotient y 100)) 7)))

#|(day-of-week 04 12 1975)
(day-of-week 04 12 2006)
(day-of-week 29 05 2013)|#

;lab 2
(define (my-gcd a b)
  (if (< a b)
      (my-gcd b a)
      (let ((r (remainder a b)))
        (if (= 0 r)
            b
            (my-gcd b r)))))

(define (my-lcm a b)
  (/ (abs (* a b)) (my-gcd a b)))

(define (prime? n)
  (define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))
  
  (= 0 (remainder (+ 1 (fact (- n 1))) n)))

#|(my-gcd 3542 2464)
(my-lcm 3 4)
(prime? 11)
(prime? 12)|#

;lab 3
(define (bisection f a b e)
  (define (sign a)
    (cond ((> a 0) 1)
          ((= a 0) 0)
          ((< a 0) -1)))
  
  (define (mid a b)
    (let ((x (+ a (/ (- b a) 2))))
      (cond ((<= (abs (f x)) e)            x)
            ((= (sign (f a)) (sign (f x))) (mid x b))
            (else                          (mid a x)))))
  
  (cond (( = 0 (f a)) a)
        (( = 0 (f b)) b)
        (else (mid a b))))

;(bisection cos -3.0 0.0 0.001)
;(bisection tan 3.0 5.0 0.001)

;lab 4
(define (newton f df x e)
  (if (< (abs (f x)) e)
      x
      (newton f df (- x (/ (f x) (df x))) e)))

(define (golden f x0 x1 e)
  (define fi (/ (+ 1 (sqrt 5)) 2))
  
  (define (loop f x0 x1 e)
    (let ((a (- x1 (/ (- x1 x0) fi)))
          (b (+ x0 (/ (- x1 x0) fi))))
      (if (>= (f a) (f b))
          (if (< (abs (- x1 a)) e)
              (/ (+ a x1) 2)
              (loop f a x1 e))
          (if (< (abs (- b x0)) e)
              (/ (+ x0 b) 2)
              (loop f x0 b e)))))
  
  (loop f x0 x1 e))

#|(round
 (newton (lambda (x) (* x x)) 
         (lambda (x) (* 2 x)) 
         1.0 1e-8))
(round
 (newton (lambda (x) (+ (* x x) (* 4 x) 4)) 
         (lambda (x) (+ (* 2 x) 4)) 
         5.0 1e-8))
(round (golden (lambda (x) (* x x)) 
               -2.0 2.0 1e-08))
(round (golden (lambda (x) (+ (* x x) (* 4 x) 4)) 
               -5.0 5.0 1e-06))|#

;lab 5

(define (my-range a b d)
  (if (< a b)
      (append (list a) (my-range (+ a d) b d))
      '()))

(define (my-flatten xs)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (if (list? x)
            (append (my-flatten x) (my-flatten (cdr xs)))
            (append (list x) (my-flatten (cdr xs)))))))

(define (my-element? x xs)
  (cond ((null? xs)          #f)
        ((equal? x (car xs)) #t)
        (else                (my-element? x (cdr xs)))))

(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (if (pred? x)
            (append (list x) (my-filter pred? (cdr xs)))
            (my-filter pred? (cdr xs))))))

#|(my-range  0 11 3)
(my-flatten '((1) 2 (3 (4 5) (6 (7 8))) 9))
(my-element? 1 '(3 2 1)) 
(my-element? 4 '(3 2 1))
(my-filter odd? (my-range 0 10 1)) 
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))|#

(define (my-fold-left op xs)
  (if (= (length xs) 2)
      (op (car xs) (cadr xs))
      (op (my-fold-left op (reverse (cdr (reverse xs)))) (car (reverse xs)))))

(define (my-fold-right op xs)
  (if (= (length xs) 2)
      (op (car xs) (cadr xs))
      (op (car xs) (my-fold-right op (cdr xs)))))

#|(my-fold-left  quotient '(16 2 2 2 2))
(my-fold-right expt     '(2 3 4))|#

;lab 6
(define (list->set xs)
  (define (helper set xs)
    (if (null? xs)
        set
        (let ((x (car xs)))
          (if (member x set)
              (helper set (cdr xs))
              (helper (cons x set) (cdr xs))))))
  
  (helper '() xs))

(define (set? xs)
  (= (length xs) (length (list->set xs))))

(define (union xs ys)
  (list->set (append xs ys)))

(define (set? xs)
  (= (length xs) (length (list->set xs))))

(define (union xs ys)
  (list->set (append xs ys)))

(define (intersection xs ys)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (if (member x ys)
            (cons x (intersection (cdr xs) ys))
            (intersection (cdr xs) ys)))))

(define (difference xs ys)
  (if (null? xs)
      '()
      (let ((x (car xs)))
        (if (member x ys)
            (difference (cdr xs) ys)
            (cons x (difference (cdr xs) ys))))))

(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))

(define (set-eq? xs ys)
  (and (= (length xs) (length ys))
       (null? (symmetric-difference xs ys))))

#|(list->set '(1 1 2 3))
(set? '(1 2 3))
(set? '(1 2 3 3))
(set? '())
(union '(1 2 3) '(2 3 4))
(intersection '(1 2 3) '(2 3 4))
(difference '(1 2 3 4 5) '(2 3))
(symmetric-difference '(1 2 3 4) '(3 4 5 6))
(set-eq? '(1 2 3) '(3 2 1))
(set-eq? '(1 2) '(1 3))

(set-eq? (list->set '(1 1 2 3))           '(1 2 3))
(set-eq? (union '(1 2 3) '(2 3 4))        '(4 3 2 1))
(set-eq? (intersection '(1 2 3) '(2 3 4)) '(3 2))
(set-eq? (difference '(1 2 3 4 5) '(2 3)) '(1 4 5))
(set-eq? (difference '(2 3) '(3 4 5))     '(2))
(set-eq? (symmetric-difference '(1 2 3 4) '(3 4 5 6)) '(6 5 2 1))|#

;lab 7
(define (string-trim-left str)
  (define (trim-left xs)
    (let ((x (car xs)))
      (if (or (eqv? x #\space)
              (eqv? x #\newline)
              (eqv? x #\tab))
          (trim-left (cdr xs))
          xs)))
  
  (list->string (trim-left (string->list str))))

(define (string-trim-right str)
  (list->string (reverse (string->list (string-trim-left (list->string (reverse (string->list str))))))))

(define (string-trim str)
  (string-trim-left (string-trim-right str)))

(define (string-prefix? a b)
  (let ((as (string->list a))
        (bs (string->list b)))
    (cond ((null? as)               #t)
          ((null? bs)               #f)
          ((eqv? (car as) (car bs)) (string-prefix? (list->string (cdr as)) (list->string (cdr bs))))
          (else                     #f))))

(define (string-suffix? a b)
  (string-prefix? (list->string (reverse (string->list a)))
                  (list->string (reverse (string->list b)))))

(define (string-infix? a b)
  (let ((bs (string->list b)))
    (cond ((null? bs)           #f)
          ((string-prefix? a b) #t)
          (else                 (string-infix? a (list->string (cdr bs)))))))

(define (string-split str sep)
  (define (trim-sep str len)
    (if (= 0 len)
        str
        (trim-sep (list->string (cdr (string->list str))) (- len 1))))
  
  (define (helper ss str)
    (cond ((null? (string->list str)) (list (list->string ss)))
          ((string-prefix? sep str)   (cons (list->string ss) (helper '() (trim-sep str (string-length sep)))))
          (else                       (helper (append ss (list (car (string->list str)))) (list->string (cdr (string->list str)))))))
  
  (helper '() str))

#|(string-trim-left  "\t\tabc def")
(string-trim-right "abc def\t")
(string-trim       "\t abc def \n")

(string-prefix? "abc" "abcdef")
(string-prefix? "bcd" "abcdef")

(string-suffix? "def" "abcdef")
(string-suffix? "bcd" "abcdef")

(string-infix? "def" "abcdefgh")
(string-infix? "abc" "abcdefgh")
(string-infix? "fgh" "abcdefgh")
(string-infix? "ijk" "abcdefgh")

(string-split "x;y;z" ";")
(string-split "x-->y-->z" "-->")|#

;lab8
(define (pack xs)
  (define (helper rs xs)
    (if (null? xs)
        (list rs)
        (let ((r (car rs))
              (x (car xs)))
          (cond ((equal? r x)     (helper (cons x rs) (cdr xs)))
                ((null? (cdr xs)) (list rs (list x)))
                (else             (cons rs (helper (list (car xs)) (cdr xs))))))))
  (if (null? xs)
      '()
      (helper (list (car xs)) (cdr xs))))

(define (encode xs)
  (define (helper xs)
    (if (null? xs)
        '()
        (append (list (list (caar xs) (length (car xs)))) (helper (cdr xs)))))
  
  (helper (pack xs)))

(define (unpack xs)
  (define (unpack-elem xs l)
    (if (= 1 l)
        xs
        (cons (car xs) (unpack-elem xs (- l 1)))))
  
  (define (helper xs)
    (if (null? xs)
        '()
        (append (list (unpack-elem (list (caar xs)) (cadar xs))) (helper (cdr xs)))))
  
  (helper xs))

(define (decode xs)
  (define (flatten a)
    (if (null? a)
        '()
        (let ((x (car a)))
          (if (list? x)
              (append x (flatten (cdr a)))
              (append (list x) (flatten (cdr a)))))))
  
  (flatten (unpack xs)))

#|(pack '(a a a f f f f b b l a a c))
(pack '((1 2) (1 2) (2 3)))
(encode '(a a a b b c))
(unpack '((a 3) (b 2) (c 1)))
(decode '((a 3) (b 2) (c 1)))|#

;lab9
(define (selection-sort pred? xs)
  (define (min-xs xs x)
    (cond ((null? xs)         x)
          ((pred? (car xs) x) (min-xs (cdr xs) (car xs)))
          (else               (min-xs (cdr xs) x))))
  
  (define (swap j xs)
    (let ((xj (list-ref xs j))
          (vs (list->vector xs)))
      (vector-set! vs j (car xs))
      (vector-set! vs 0 xj)
      (vector->list vs)))
  
  (define (ind x xs)
    (- (length xs) (length (member x xs))))
  
  (define (helper xs)
    (if (null? xs)
        '()
        (let ((x (min-xs xs (car xs))))
          (cons x (helper (cdr (swap (ind x xs) xs)))))))
  
  (helper xs))

(define (insertion-sort pred? xs)
  (define (insert xs ys x)
    (cond ((null? ys) (append xs (list x)))
          ((pred? (car ys) x) (insert (append xs (list (car ys))) (cdr ys) x))
          (else               (append xs (list x) ys))))
  
  (define (helper xs ys)
    (if (null? ys)
        xs
        (helper (insert '() xs (car ys)) (cdr ys))))
  
  (helper '() xs))

#|(selection-sort <= '(9 6 2 4 3 5 7 1 8 0))
(insertion-sort <= '(9 6 2 4 3 5 7 1 8 0))|#

;lab 10
(define (make-vector-size sizes fill)
  (define (helper xs size)
    (if (= (length xs) size)
        (list->vector (cons 'multi-vector (cons (length sizes) (append sizes xs))))
        (helper (cons fill xs) size)))
  
  (helper '() (my-fold-right * sizes)))

(define-syntax make-multi-vector
  (syntax-rules ()
    ((_ sizes)      (make-vector-size sizes 0))
    ((_ sizes fill) (make-vector-size sizes fill))))

(define (multi-vector? m)
  (define (take-mul num xs)
    (if (= 0 num)
        1
        (* (car xs) (take-mul (- num 1) (cdr xs)))))
  
  (and (vector? m)
       (> (vector-length m) 2)
       (eqv? (vector-ref m 0) 'multi-vector)
       (> (vector-length m) (+ 2 (vector-ref m 1)))
       (= (vector-length m) (+ 2 (vector-ref m 1) (take-mul (vector-ref m 1) (cddr (vector->list m)))))))

(define (take-lens m)
  (define (helper xs ms)
    (if (= (length xs) (vector-ref m 1))
        xs
        (helper (append xs (list (car ms))) (cdr ms))))
  
  (helper '() (cddr (vector->list m))))

(define (get-index indices xs)
  (define (helper indx xs)
    (if (null? xs)
        (+ 2 (length indices) (car indx))
        (+ (my-fold-right * (cons (car indx) xs)) (helper (cdr indx) (cdr xs)))))
  
  (helper indices xs))

(define (multi-vector-ref m indices)
  (and (multi-vector? m)
       (= (length indices) (vector-ref m 1))
       (vector-ref m (get-index indices (cdr (take-lens m))))))

(define (multi-vector-set! m indices x)
  (and (multi-vector? m)
       (= (length indices) (vector-ref m 1))
       (vector-set! m (get-index indices (cdr (take-lens m))) x)))

#|(define m (make-multi-vector '(11 12 9 16)))
(multi-vector? m)
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12))

(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0))

(define m (make-multi-vector '(2 4 2)))
(multi-vector-set! m '(0 0 0) 0)
(multi-vector-set! m '(0 0 1) 1)
(multi-vector-set! m '(0 1 0) 2)
(multi-vector-set! m '(0 1 1) 3)
(multi-vector-set! m '(0 2 0) 4)
(multi-vector-set! m '(0 2 1) 5)
(multi-vector-set! m '(0 3 0) 6)
(multi-vector-set! m '(0 3 1) 7)
(multi-vector-set! m '(1 0 0) 8)
(multi-vector-set! m '(1 0 1) 9)
(multi-vector-set! m '(1 1 0) 10)
(multi-vector-set! m '(1 1 1) 11)
(multi-vector-set! m '(1 2 0) 12)
(multi-vector-set! m '(1 2 1) 13)
(multi-vector-set! m '(1 3 0) 14)
(multi-vector-set! m '(1 3 1) 15)
m|#

;lab 11
(define (char->dec char)
  (let ((int (char->integer char)))
    (cond ((and (>= int (char->integer #\a))
                (<= int (char->integer #\z)))
           (+ 10 (- int (char->integer #\a))))
          ((and (>= int (char->integer #\A))
                (<= int (char->integer #\Z)))
           (+ 10 (- int (char->integer #\A))))
          (else
           (- int (char->integer #\0))))))

(define (dec->char dec)
  (if (< dec 10)
      (integer->char (+ (char->integer #\0) dec))
      (integer->char (+ (- dec 10) (char->integer #\a)))))

(define (correct-number? s b)
  (define (correct-string? ls)
    (or (null? ls)
        (and (or (and (>= (char->integer (car ls)) (char->integer #\a))
                      (<= (char->integer (car ls)) (char->integer #\z)))
                 (and (>= (char->integer (car ls)) (char->integer #\A))
                      (<= (char->integer (car ls)) (char->integer #\Z)))
                 (and (>= (char->integer (car ls)) (char->integer #\0))
                      (<= (char->integer (car ls)) (char->integer #\9))))
             (correct-string? (cdr ls)))))
  
  (define (helper ls)
    (or (null? ls)
        (and (< (char->dec (car ls)) b)
             (helper (cdr ls)))))
  
  (and (string? s)
       (not (null? s))
       (correct-string? (string->list s))
       (helper (string->list s))))

(define (certain->decimal s b)
  (define (helper p ls)
    (if (null? ls)
        0
        (+ (* (char->dec (car ls)) p) (helper (* p b) (cdr ls)))))
  
  (if (correct-number? s b)
      (helper 1 (reverse (string->list s)))
      'number-conversion-error))

(define (decimal->certain d b)
  (define (helper d)
    (let ((r (list (dec->char (remainder d b))))
          (q (quotient d b)))
      (if (= 0 q)
          r
          (append (helper q) r))))
  
  (list->string (helper d)))

;; tests
#|(certain->decimal "FA0E" 16)
(certain->decimal "ZZ" 36)
(certain->decimal "ZZ" 35)

(correct-number? "ab0" 12)
(correct-number? "ab0" 10)

(decimal->certain 64014 16)
(decimal->certain 1295 36)|#

;lab 12
(define (factorize expr)
  (define (expr-expt? expr pow)
    (and (list? expr)
         (= (length expr) 3)
         (eqv? (car expr) 'expt)
         (or (symbol? (cadr expr))
             (list? (cadr expr))
             (number? (cadr expr)))
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
                (list '-
                      (list 'expt a 2)
                      (list '* a b))
                (list 'expt b 2))))
  
  (define (factorize-sub-sqr a b)
    (list '*
          (list '- a b)
          (list '+ a b)))
  
  (let ((expt-a (cadr expr))
        (expt-b (caddr expr)))
    (cond ((and (eqv? (car expr) '-) (expr-expt? expt-a 3) (expr-expt? expt-b 3))
           (factorize-sub-cub (cadr expt-a) (cadr expt-b)))
          ((and (eqv? (car expr) '+) (expr-expt? expt-a 3) (expr-expt? expt-b 3))
           (factorize-add-cub (cadr expt-a) (cadr expt-b)))
          ((and (eqv? (car expr) '-) (expr-expt? expt-a 2) (expr-expt? expt-b 2))
           (factorize-sub-sqr (cadr expt-a) (cadr expt-b))))))

;; tests
#|(define expr '(- (expt (- a dc) 3) (expt b 3)))
(factorize expr)
(factorize '(- (expt x 2) (expt y 2)))

(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))

(eval (list (list 'lambda 
                  '(x y) 
                  (factorize '(- (expt x 2) (expt y 2))))
            1 2)
      (interaction-environment))|#

;lab 13
(define (get-power p)
  (let ((x (if (list? p)
               (append (list '-) p (list 1))
               (list '- p 1))))
    (eval x (interaction-environment))))

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
  
  (derivative-simple expr))

;; tests
#|(derivative '(2))
(derivative '(x))
(derivative '(- x))
(derivative '(* 1 x))
(derivative '(- (* 1 x)))
(derivative '(expt x 10))
(derivative '(* 2 (expt x 5)))
(derivative '(expt x -2))
(derivative '(cos x))
(derivative '(* 2 (expt 2 x)))
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
(derivative '(cos (* 2 (expt x 2))))|#

;lab 14
(define-syntax when
  (syntax-rules ()
    ((_ cond? expr . exprs)
     (if cond? (begin expr . exprs)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond? expr . exprs)
     (if (not cond?) (begin expr . exprs)))))

(define-syntax for
  (syntax-rules (in as)
    ((_ item in items body)
     (for-each (lambda (item) body) items))
    ((_ items as item body)
     (for item in items body))))

;; tests
(define x 1)
(when   (> x 0) (display "x > 0")  (newline))
(unless (= x 0) (display "x != 0") (newline))

(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline))) 

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))