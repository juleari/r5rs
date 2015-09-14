(define (day-of-week day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (- (+ month (* a 12)) 2)))
    (remainder (- (+ 7000 day y (quotient y 4) (quotient y 400) (quotient (* 31 m) 12)) (quotient y 100)) 7)))

;; tests
(day-of-week 04 12 1975)
(day-of-week 04 12 2006)
(day-of-week 29 05 2013)