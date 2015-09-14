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
              (append (flatten x) (flatten (cdr a)))
              (append (list x) (flatten (cdr a)))))))
  
  (flatten (unpack xs)))

;; tests
(pack '(a a a f f f f b b l a a c))
(pack '((1 2) (1 2) (2 3)))
(encode '(a a a b b c))
(unpack '((a 3) (b 2) (c 1)))
(decode '((a 3) (b 2) (c 1)))