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
  (list->string
   (reverse (string->list (string-trim-left (list->string (reverse (string->list str))))))))

(define (string-trim str)
  (string-trim-left (string-trim-right str)))

(define (string-prefix? a b)
  (let ((as (string->list a))
        (bs (string->list b)))
    (or (null? as)
        (and (not (null? bs))
             (eqv? (car as) (car bs))
             (string-prefix? (list->string (cdr as)) (list->string (cdr bs)))))))

(define (string-suffix? a b)
  (string-prefix? (list->string (reverse (string->list a)))
                  (list->string (reverse (string->list b)))))

(define (string-infix? a b)
  (let ((bs (string->list b)))
    (and (not (null? bs))
         (or (string-prefix? a b)
             (string-infix? a (list->string (cdr bs)))))))

(define (string-split str sep)
  (define (trim-sep str len)
    (if (= 0 len)
        str
        (trim-sep (list->string (cdr (string->list str))) (- len 1))))
  
  (define (helper ss str)
    (cond ((null? (string->list str)) (list (list->string ss)))
          ((string-prefix? sep str)   (cons (list->string ss)
                                            (helper '() (trim-sep str (string-length sep)))))
          (else                       (helper (append ss (list (car (string->list str))))
                                              (list->string (cdr (string->list str)))))))
  
  (helper '() str))

;; tests
(string-trim-left  "\t\tabc def")
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
(string-split "x-->y-->z" "-->")