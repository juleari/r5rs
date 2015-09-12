(define (newton f df x e)
  (if (< (abs (f x)) e)
      x
      (newton f df (- x (/ (f x) (df x))) e)))

(define (golden f x0 x1 e)
  (define fi (/ (+ 1 (sqrt 5)) 2))
  
  (define (loop f x0 x1 e)
    (let ((a (- x1 (/ (- x1 x0) fi)))
          (b (+ x0 (/ (- x1 x0) fi))))
      (if (>= (f a) (f b))
          (if (< (abs (- x1 a)) e)
              (/ (+ a x1) 2)
              (loop f a x1 e))
          (if (< (abs (- b x0)) e)
              (/ (+ x0 b) 2)
              (loop f x0 b e)))))
  
  (loop f x0 x1 e))

(round
 (newton (lambda (x) (* x x)) 
         (lambda (x) (* 2 x)) 
         1.0 1e-8))
(round
 (newton (lambda (x) (+ (* x x) (* 4 x) 4)) 
         (lambda (x) (+ (* 2 x) 4)) 
         5.0 1e-8))
(round (golden (lambda (x) (* x x)) 
               -2.0 2.0 1e-08))
(round (golden (lambda (x) (+ (* x x) (* 4 x) 4)) 
               -5.0 5.0 1e-06))