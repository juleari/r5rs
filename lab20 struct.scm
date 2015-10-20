(define (make-fields% cols pairs)
  (if (null? cols)
      (reverse pairs)
      (let* ((col  (car cols))
             (colb `,col))
        (make-fields% (cdr cols) (cons `(,col ,colb) pairs)))))

(define (fill-fields% name cols pairs)
  (write 1))

(define (define-make% name cols)
  (let* ((mname (string->symbol (string-append "make-" (symbol->string name))))
         (mdef  (cons mname cols))
         (mbody `(vector ',name ',(make-fields% cols '()))))
    (write `(define ,mdef ,mbody))
    (eval `(define ,mdef ,mbody)(interaction-environment))))


(define (fields% p f)
  (or (and (null? p)
           (null? f))
      (and (not (null? p))
           (not (null? f))
           (not (null? (car p)))
           (eq? (caar p) (car f))
           (fields% (cdr p) (cdr f)))))

(define (check? p cols)
  (and (not (null? p))
       (eq? 'pos (car p))
       (fields% (cdr p) cols)))

(define (define-pred% name cols)
  
  (let* ((pname (string->symbol (string-append (symbol->string name) "?")))
         (pdef  (list pname 'p)))
    (eval `(define ,pdef (and (vector? p)
                              (check? (vector->list p) ',cols)))
          (interaction-environment))))

(define (define-gets% name cols) 1)
(define (define-sets% name cols) 1)

(define (define-struct% name cols)
  (define-make% name cols)
  (define-pred% name cols)
  (define-gets% name cols)
  (define-sets% name cols))

(define-syntax define-struct
  (syntax-rules ()
    ((_ name cols) (define-struct% (quote name) (quote cols)))))

;; tests
(define-struct tr (col1 col2))
;(define t (make-tr 1 2))
;(tr? t)



(define (make-pos row col)
  (vector 'pos `((row ,row) (col ,col))))

(define (fields p f)
  (or (and (null? p)
           (null? f))
      (and (not (null? p))
           (not (null? f))
           (not (null? (car p)))
           (eq? (caar p) (car f))
           (fields (cdr p) (cdr f)))))

(define (check? p)
  (and (not (null? p))
       (eq? 'pos (car p))
       (fields (cdr p) '(row col))))

(define (pos? p)
  (and (vector? p)
       (check? (vector->list p))))

(define (pos-row p)
  (cadr (assq 'row (cdr (vector->list p)))))

(define (pos-col p)
  (cadr (assq 'col (cdr (vector->list p)))))

;(define (set-pos-row! p v)
;  (set! ()))

;(define p (make-pos 1 2))
;(pos? p)
;(pos-row p)
;(pos-col p)