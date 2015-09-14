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

(list->set '(1 1 2 3))
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
(set-eq? (symmetric-difference '(1 2 3 4) '(3 4 5 6)) '(6 5 2 1))