;(use-syntax (ice-9 syncase))

(define (concat-symbols a . b)
  (define (helper x xs)
    (if (null? xs)
        x
        (helper (string->symbol (string-append (symbol->string x)
                                               (symbol->string (car xs))))
                (cdr xs))))
  (helper a b))

(define (set-assoc! alist key value)
  (if (null? alist)
      (list (list key value))
      (let ((a (car alist)))
        (if (equal? (car a) key)
            (cons (list key value) (cdr alist))
            (cons a (set-assoc! (cdr alist) key value))))))

(define (define-make% name cols)
  (let* ((mname (concat-symbols 'make- name)))
    (eval `(define (,mname . vals) (map (lambda (col val) `(,col ,val)) ',cols vals))
          (interaction-environment))))

(define (fields% p f)
  (or (and (null? p)
           (null? f))
      (and (not (null? p))
           (not (null? f))
           (not (null? (car p)))
           (eq? (caar p) (car f))
           (fields% (cdr p) (cdr f)))))

(define (define-pred% name cols)
  (let* ((pname (concat-symbols name '?))
         (pdef  (list pname 'p)))
    (eval `(define ,pdef (and (list? p)
                              (fields% p ',cols)))
          (interaction-environment))))

(define (define-gets% name cols)
  (map (lambda (col)
         (let* ((gname (concat-symbols name '- col))
                (gdef  (list gname 'p)))
           (eval `(define ,gdef (cadr (assoc ',col p)))
                 (interaction-environment))))
       cols))

(define (define-sets% name cols)
  (map (lambda (col)
         (let* ((sname (concat-symbols 'set- name '- col '!))
                (sdef  (list sname 'p 'v)))
           (eval `(define-syntax ,sname
                    (syntax-rules ()
                      ((_ p v) (set! p (set-assoc! p ',col v)))))
                 (interaction-environment))))
       cols))

(define (define-struct% name cols)
  (define-sets% name cols)
  (define-gets% name cols)
  (define-pred% name cols)
  (define-make% name cols))

(define-syntax define-struct
  (syntax-rules ()
    ((_ name cols) (define-struct% (quote name) (quote cols)))))

;; tests
#|(define-struct tr (col1 col2))
(define t (make-tr 1 2))
(tr? t)

(define-struct pos (row col))

(define p (make-pos 1 2))
(pos? p)

(pos-row p)
(pos-col p)

(set-pos-row! p 3)
(set-pos-col! p 4)

(pos-row p)
(pos-col p)|#