; Написать процедуру (palindrom? xs), которая будет проверять является ли список палиндромом
; (Палиндромом считается список, который с конца читается также как и сначала)
(define (palindrom? xs)
  (equal? xs (reverse xs)))

(palindrom? '((1 2 3) 1 (1 2 3))) ; #t
(palindrom? '((1 2 3) 1 (3 2 1))) ; #f

; Написать процедуру (palindrom-req? xs), которая будет проверять является ли содержимое списка палиндромом
; palindrom-req1
(define (palindrom-req? xs)
  (define (helper xs ys)
    (or (and (null? xs)
             (null? ys))
        (and (list? (car xs))
             (list? (car ys))
             (helper (car xs) (reverse (car ys)))
             (helper (cdr xs) (cdr ys)))
        (and (not (list? (car xs)))
             (equal? (car xs) (car ys))
             (helper (cdr xs) (cdr ys)))))
  (helper xs (reverse xs)))

; palindrom-req2
(define (palindrom-req? xs)
  (define (my-flatten xs)
    (or (and (null? xs)
             '())
        (and (list? (car xs))
             (append (my-flatten (car xs)) (my-flatten (cdr xs))))
        (cons (car xs) (my-flatten (cdr xs)))))
  
  (palindrom? (my-flatten xs)))

(palindrom-req? '((1 2 3) 1 (1 2 3))) ; #f
(palindrom-req? '((1 2 3) 1 (3 2 1))) ; #t

; Написать процедуру (count pred? xs) которая будет возвращать количество элементов списка xs,
; удовлетворяющих условию pred?
(define (count pred? xs)
  (or (and (null? xs)
           0)
      (and (pred? (car xs))
           (+ 1 (count pred? (cdr xs))))
      (count pred? (cdr xs))))

(count odd? '(1 2 3 4 5 6 7)) ; 4

; Написать процедуру add, которая будет складывать произвольное количество элементов.
; add1
(define (add . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply add (cdr xs)))))

; add2
(define (add . xs)
  (apply + xs))

(add)           ; 0
(add 1 4 6 2 8) ; 21

; Написать процедуру (polynom as x), которая будет вычислять значение полинома (a0*x^n + ... + an)
; polynom1
(define (polynom as x)
  (define (helper as n)
    (if (null? as)
        0
        (+ (* (car as) (expt x n))
           (helper (cdr as) (+ 1 n)))))
  
  (helper as 0))

; polynom2
(define (polynom as x)
  (define (helper as y)
    (if (null? as)
        0
        (+ (* (car as) y)
           (helper (cdr as) (* y x)))))
  
  (if (null? as)
      0
      (helper as 1)))

; polynom3
(define (polynom as x)
  (define (helper as sum)
    (if (null? as)
        sum
        (helper (cdr as)
                 (+ (* sum x)
                    (car as)))))
  (helper (reverse as) 0))

(polynom '(3 2) 10)
(polynom '(-4 1 -5 2 3) 2)