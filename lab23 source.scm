;(use-syntax (ice-9 syncase))

(define-syntax make-source
  (syntax-rules ()
    ((_ sequence end) (let ((lend (list end)))
                        (cond ((string? sequence) (append (string->list sequence) lend))
                              ((vector? sequence) (append (vector->list sequence) lend))
                              (else               (append sequence lend)))))
    ((_ sequence)     (make-source sequence #f))))

(define-syntax next
  (syntax-rules ()
    ((_ src) (if (null? (cdr src))
                 (car src)
                 (let ((s (car src)))
                   (set! src (cdr src))
                   s)))))

(define (peek src) (car src))

#|
(define s (make-source "abc"))
(peek s) ; ⇒ #\a  ; "остаток" ≡ "abc"
(next s) ; ⇒ #\a  ; "остаток" ≡ "bc"
(next s) ; ⇒ #\b  ; "остаток" ≡ "c"
(next s) ; ⇒ #\c  ; "остаток" ≡ ""
(next s) ; ⇒ #t   ; "остаток" ≡ ""
(peek s)

(define v (make-source #(1 2 3) 0))

(peek v) ; ⇒ 1  ; "остаток" ≡ #(1 2 3)
(next v) ; ⇒ 1  ; "остаток" ≡ #(2 3)
(next v) ; ⇒ 2  ; "остаток" ≡ #(3)
(next v) ; ⇒ 3  ; "остаток" ≡ #()
(next v) ; ⇒ 0  ; "остаток" ≡ #()
(peek v) ; ⇒ 0  ; "остаток" ≡ #()

;; Работа со списком как с последовательностью символов

(define l (make-source '(one two three) 'end))
;
; Признак конца строки по-умолчанию установлен равным end

(peek l) ; ⇒ one    ; "остаток" ≡ (one two three)
(next l) ; ⇒ one    ; "остаток" ≡ (two three)
(next l) ; ⇒ two    ; "остаток" ≡ (three)
(next l) ; ⇒ three  ; "остаток" ≡ ()
(next l) ; ⇒ end    ; "остаток" ≡ ()
(peek l) ; ⇒ end    ; "остаток" ≡ ()|#