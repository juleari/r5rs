;; digit   ::= [0-9]
;; integer ::= [+-]? digit+

(define (check-integer str)
  (and (scan-integer str) (string? str)))

(define (scan-integer str)
  (define (digit? x)
    (let ((ordx (char->integer x))
          (ord0 (char->integer #\0))
          (ord9 (char->integer #\9)))
      (and (>= ordx ord0)
           (<= ordx ord9)
           (-  ordx ord0))))
  
  (define (helper xs int b digit)
    (if (null? xs)
        int
        (let* ((x (car xs))
               (s (cdr xs))
               (d (digit? x)))
          (or (and (eq? x #\+) digit (null? s) digit int)
              (and (eq? x #\-) digit (null? s) (- int))
              (and d (helper s (+ (* d b) int) (* b 10) #t))))))
  
  (and (string? str)
       (not (eq? str ""))
       (helper (reverse (string->list str)) 0 1 #f)))

(define (scan-many-integers str)
  (define (append-x x xs)
    (if (not (null? x))
            (cons (list->string (reverse x)) xs)
            xs))
  
  (define (separator str x xs)
    (if (null? str)
        (reverse (append-x x xs))
        (let ((s (car str))
              (t (cdr str)))
          (if (or (eqv? s #\space)
                  (eqv? s #\newline)
                  (eqv? s #\tab))
              (separator t '() (append-x x xs))
              (separator t (cons s x) xs)))))
  
  (define (map-int xs)
    (define (helper xs ints)
      (if (null? xs)
          (and (not (null? ints)) (reverse ints))
          (let* ((x (car xs))
                 (s (cdr xs))
                 (i (scan-integer x)))
            (and i (helper s (cons i ints))))))
    
    (helper xs '()))
  
  (map-int (separator (string->list str) '() '())))

;; tests
#|(check-integer "+123")       ; ⇒ #t
(check-integer "-123")       ; ⇒ #t
(check-integer "1234")       ; ⇒ #t
(check-integer "1")          ; ⇒ #t
(check-integer "1/2")        ; ⇒ #f

(scan-integer "-123")        ; ⇒ -123
(scan-integer "1234")        ; ⇒ 1234
(scan-integer "1")           ; ⇒ 1
(scan-integer "1/2")         ; ⇒ #f

(scan-many-integers "\t1\t\t-2\n3") 
; ⇒ (1 -2 3)
(scan-many-integers "1+ 1")  ; ⇒ #f|#