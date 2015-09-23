; polindrom?
(define (polindrom? xs)
  (equal? xs (reverse xs)))

(polindrom? '((1 2 3) 1 (1 2 3))) ; #t
(polindrom? '((1 2 3) 1 (3 2 1))) ; #f

; polindrom-req1
(define (polindrom-req? xs)
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

; polindrom-req2
(define (polindrom-req? xs)
  (define (my-flatten xs)
    (or (and (null? xs)
             '())
        (and (list? (car xs))
             (append (my-flatten (car xs)) (my-flatten (cdr xs))))
        (cons (car xs) (my-flatten (cdr xs)))))

  (polindrom? (my-flatten xs)))

(polindrom-req? '((1 2 3) 1 (1 2 3))) ; #f
(polindrom-req? '((1 2 3) 1 (3 2 1))) ; #t

; count pred?
(define (count pred? xs)
  (or (and (null? xs)
           0)
      (and (pred? (car xs))
           (+ 1 (count pred? (cdr xs))))
      (count pred? (cdr xs))))

(count odd? '(1 2 3 4 5 6 7)) ; 4

; add1
(define (add . xs)
  (or (and (null? xs)
           0)
      (+ (car xs) (apply add (cdr xs)))))

; add2
(define (add . xs)
  (apply + xs))

(add)           ; 0
(add 1 4 6 2 8) ; 21