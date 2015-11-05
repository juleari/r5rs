(define (my-gcd a b)
  (if (< a b)
      (my-gcd b a)
      (let ((r (remainder a b)))
        (if (zero? r)
            b
            (my-gcd b r)))))

(define (my-lcm a b)
  (/ (abs (* a b)) (my-gcd a b)))

(define (prime? n)
  (define (fact n)
    (if (zero? n)
        1
        (* n (fact (- n 1)))))
  
  (zero? (remainder (+ 1 (fact (- n 1))) n)))

;; tests
(my-gcd 3542 2464)
(my-lcm 3 4)
(prime? 11)
(prime? 12)
(prime? 3571)