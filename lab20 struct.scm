;(use-syntax (ice-9 syncase))

(define (concat-symbols . a)
  (string->symbol (apply string-append (map symbol->string a))))

(define (ids i cols)
  (if (null? cols)
      '()
      (cons i (ids (+ 1 i) (cdr cols)))))

(define (define-make% name cols)
  (let* ((mname (concat-symbols 'make- name)))
    (eval `(define (,mname . vals)
             (list->vector (cons ',name
                                 (map (lambda (val) val) vals))))
          (interaction-environment))))

(define (define-pred% name cols)
  (let* ((pname (concat-symbols name '?))
         (pdef  (list pname 'p)))
    (eval `(define ,pdef (and (vector? p)
                              (eq? (vector-ref p 0) ',name)
                              (eq? (vector-length p) (+ 1 (length ',cols)))))
          (interaction-environment))))

(define (define-gets% name cols)
  (map (lambda (col i)
         (let* ((gname (concat-symbols name '- col))
                (gdef  (list gname 'p)))
           (eval `(define ,gdef (vector-ref p ,i))
                 (interaction-environment))))
       cols (ids 1 cols)))

(define (define-sets% name cols)
  (map (lambda (col i)
         (let* ((sname (concat-symbols 'set- name '- col '!))
                (sdef  (list sname 'p 'v)))
           (eval `(define ,sdef (vector-set! p ,i v))
                 (interaction-environment))))
       cols (ids 1 cols)))

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