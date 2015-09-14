(define (bisection f a b e)
  (define (sign a)
    (cond ((> a 0) 1)
          ((= a 0) 0)
          ((< a 0) -1)))
  
  (define (mid a b)
    (let ((x (+ a (/ (- b a) 2))))
      (cond ((<= (abs (f x)) e)            x)
            ((= (sign (f b)) (sign (f x))) (mid a x))
            (else                          (mid x b)))))
  
  (cond (( = 0 (f a)) a)
        (( = 0 (f b)) b)
        (else (mid a b))))

;; tests
(bisection cos -3.0 0.0 0.001)