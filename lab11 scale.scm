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
(certain->decimal "FA0E" 16)
(certain->decimal "ZZ" 36)
(certain->decimal "ZZ" 35)

(correct-number? "ab0" 12)
(correct-number? "ab0" 10)

(decimal->certain 64014 16)
(decimal->certain 1295 36)