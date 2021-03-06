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

;; tests
(selection-sort <= '(9 6 2 4 3 5 7 1 8 0))
(insertion-sort <= '(9 6 2 4 3 5 7 1 8 0))