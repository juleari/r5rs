(define (interpret program stack)
  (define (op? x)
    (or (eq? x '+) (eq? x '-) (eq? x '*) (eq? x 'or) (eq? x 'and)))
  
  (define (comp? x)
    (or (eq? x '=) (eq? x '<) (eq? x '>)))
  
  (define (find-end i end)
    (if (eq? (vector-ref program i) end)
        (+ i 1)
        (find-end (+ i 1) end)))
  
  (define (scan i stack return dict)
    (if (eq? i (vector-length program))
        stack
        (let* ((word (vector-ref program i))
               (func (assoc word dict)))
                ;; Выполнение программы ;;
          (cond (func (scan (cadr func) stack (cons (+ i 1) return) dict))
                ((number? word) (scan (+ 1 i) (cons word stack) return dict))
                ;; Управляющие конструкции ;;
                ((eq? word 'define) (scan (find-end i 'end) stack return
                                          (cons (list (vector-ref program (+ 1 i)) (+ 2 i)) dict)))
                ((or (eq? word 'end) (eq? word 'exit)) (scan (car return) stack (cdr return) dict))
                ((eq? word 'if) (if (zero? (car stack))
                                    (scan (find-end i 'endif) (cdr stack) return dict)
                                    (scan (+ 1 i) (cdr stack) return dict)))
                ((eq? word 'endif) (scan (+ 1 i) stack return dict))
                ;; Операции с числами ;;
                ((op? word) (scan (+ 1 i) 
                                  (eval `(cons (,word (cadr (list ,@stack)) (car (list ,@stack)))
                                               (cddr (list ,@stack)))
                                        (interaction-environment))
                                  return dict))
                ((eq? word '/) (scan (+ 1 i)
                                       (cons (quotient (cadr stack) (car stack)) (cddr stack))
                                       return dict))
                ((eq? word 'mod) (scan (+ 1 i)
                                       (cons (remainder (cadr stack) (car stack)) (cddr stack))
                                       return dict))
                ((eq? word 'neg) (scan (+ 1 i) (cons (- (car stack)) (cdr stack)) return dict))
                ((comp? word) (scan (+ 1 i) 
                                    (eval `(cons (if (,word (cadr (list ,@stack))
                                                            (car (list ,@stack))) -1 0)
                                                 (cddr (list ,@stack)))
                                          (interaction-environment)) return dict))
                ((eq? word 'not) (scan (+ 1 i) (cons (not (car stack)) (cdr stack)) return dict))
                ;; Операции со стеком ;;
                ((eq? word 'drop) (scan (+ 1 i) (cdr stack) return dict))
                ((eq? word 'swap) (scan (+ 1 i) (append (list (cadr stack) (car stack))
                                                        (cddr stack)) return dict))
                ((eq? word 'dup)  (scan (+ 1 i) (cons (car stack) stack) return dict))
                ((eq? word 'over) (scan (+ 1 i) (cons (cadr stack) stack) return dict))
                ((eq? word 'rot)  (scan (+ 1 i) (append (list (caddr stack) (cadr stack) (car stack))
                                                        (cdddr stack)) return dict))
                ((eq? word 'depth) (scan (+ 1 i) (cons (length stack) stack) return dict))))))
  (scan 0 stack '() '((#f #f))))

;; tests
#|(interpret #(define abs
              dup 0 <
              if neg endif
             end
              9 abs
              -9 abs) '())

(interpret #(   define =0? dup 0 = end 
                define <0? dup 0 < end 
                define signum 
                    =0? if exit endif 
                    <0? if drop -1 exit endif 
                    drop 
                    1 
                end 
                 0 signum 
                -5 signum 
                10 signum       ) (quote ()))

(interpret #(   define -- 1 - end 
                define =0? dup 0 = end 
                define =1? dup 1 = end 
                define factorial 
                    =0? if drop 1 exit endif 
                    =1? if drop 1 exit endif 
                    dup -- 
                    factorial 
                    * 
                end 
                0 factorial 
                1 factorial 
                2 factorial 
                3 factorial 
                4 factorial     ) (quote ()))

(interpret #(   define =0? dup 0 = end 
                define =1? dup 1 = end 
                define -- 1 - end 
                define fib 
                    =0? if drop 0 exit endif 
                    =1? if drop 1 exit endif 
                    -- dup 
                    -- fib 
                    swap fib 
                    + 
                end 
                define make-fib 
                    dup 0 < if drop exit endif 
                    dup fib 
                    swap -- 
                    make-fib 
                end 
                10 make-fib     ) (quote ()))

(interpret #(   define =0? dup 0 = end 
                define gcd 
                    =0? if drop exit endif 
                    swap over mod 
                    gcd 
                end 
                90 99 gcd 
                234 8100 gcd    ) '())|#