(define (terminal? x)
  (or (eqv? x #\space)
      (eqv? x #\newline)
      (eqv? x #\tab)))

(define (read-words)
  (define (add-word word words)
    (if (null? word)
        words
        (cons (list->string (reverse word)) words)))
  
  (define (helper word words)
    (let ((ch (read-char)))
      (if (eof-object? ch)
          (reverse (add-word word words))
          (if (terminal? ch)
              (helper '() (add-word word words))
              (helper (cons ch word) words)))))
  
  (helper '() '()))
