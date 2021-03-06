#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "schemeinscheme.rkt")

(define-test-suite part1-tests
  (test-case
   "basic-expressions"
   (check-equal? (my-eval 1) 1)
   (check-equal? (my-eval "hello") "hello")
   (check-equal? (my-eval #t) #t)
   (check-equal? (my-eval #f) #f)

   ;; quote
   (check-equal? (my-eval '(my-quote symbol)) 'symbol )
   (check-equal? (my-eval '(my-quote (foobar 1 2))) '(foobar 1 2) )
   (check-equal? (my-eval '(my-quote (my-+ 1 2))) '(my-+ 1 2) )
   )

  (test-case
   "if test cases"
   (check-equal? (my-eval '(my-if #t 1 2)) 1 )
   (check-equal? (my-eval '(my-if #f 1 2)) 2 )
   (check-equal? (my-eval '(my-if (my-if #t #f #t) 3 4)) 4 )
   (check-equal? (my-eval '(my-if #f 1 (my-if #t 3 4))) 3)
   )

  (test-case
   "let test cases"
   (check-equal? (my-eval 'null) null)

   ;; ok now lets try some lets
   (check-equal? (my-eval '(my-let ((x 1)(y 2)) x)) 1)
   (check-equal? (my-eval '(my-let ((x 1)(y 2)) y)) 2)
   (check-equal? (my-eval '(my-let ((x 1)(y 2)) (my-let ((x 3) (z 4)) z ))) 4)
   (check-equal? (my-eval '(my-let ((x 1)(y 2)) (my-let ((x 3) (z 4)) x ))) 3)
   (check-equal? (my-eval '(my-let ((a 99))(my-let ((x 1)(y a)) y))) 99)
   )
  
  (test-case
   "lambda test cases"
   (check-equal? (my-eval '((my-lambda (x y) 99) 1 2)) 99)
   (check-equal? (my-eval '((my-lambda (x y) x) 1 2)) 1)
   (check-equal? (my-eval '((my-lambda (x y) y) 1 2)) 2)
   (check-equal? (my-eval '(my-let ((x 3) (y 4)) ((my-lambda (x y) y) y x))) 3)
   (check-equal? (my-eval '(my-let ((func (my-let ((val 3)) (my-lambda (x) (my-+ val x))))) (func 100))) 103)
   )

  (test-case
   "my+ test cases"
   ;; plus prim
   (check-equal? (my-eval '(my-+ 1 2 3)) 6)
   (check-equal? (my-eval '(my-let ((x 3) (y 4)) (my-+ x y))) 7)
   ;; note that it should be posibile to treat plus as an ordinary function
   (check-equal? (my-eval '(my-let ((x 3) (y 4) (func my-+)) (func x y))) 7)              

   ;; this one is actually more a test of let, but it was hard to do shadowing
   ;; without 2 parameter operations

   (check-equal? (my-eval '(my-let ((x 3) (y 4)) (my-+ (my-let ((x 10)) x) x))) 13)
  )

  (test-case
   "other prims"
   (check-equal? (my-eval '(my-- 10 2)) 8)
   (check-equal? (my-eval '(my-* 10 2)) 20)
   (check-equal? (my-eval '(my-/ 10 2)) 5)
   (check-equal? (my-eval '(my-cons 10 (my-cons 11 null))) '(10 11))
   (check-equal? (my-eval '(my-car (my-quote (1 2 3)))) 1)
   (check-equal? (my-eval '(my-cdr (my-quote (1 2 3)))) '(2 3))
   (check-equal? (my-eval '(my-null? null)) #t)
   (check-equal? (my-eval '(my-null? (my-quote (1 2 3)))) #f)
   (check-equal? (my-eval '(my-eq? 1 2)) #f)
   (check-equal? (my-eval '(my-eq? 1 1)) #t)
   ;; we won't bother with unit tests for display, but it's
   ;; useful for your tests right?
   )

  (test-case
   "recursive factorial"
   (check-equal? (my-eval
               '(my-let ((simple-fact
                          (my-lambda (recurse val)
                                     (my-if (my-eq? val 0) 1
                                            (my-* val (recurse recurse (my-- val 1)))))))
                        (simple-fact simple-fact 4)))
              24))
)


(define-test-suite part2-tests
  (test-case
   "simple terms and factors"
   (check-equal? (parse '(1)) 1)
   (check-equal? (parse '(hello)) 'hello)

   (check-equal? (parse '(1 * q)) '(my-* 1 q)))

  (test-case
   "recursive term"
   (check-equal? (parse '(1 * q * 2)) '(my-* 1 (my-* q 2))))

  (test-case
   "recusive expressions"
   (check-equal? (parse '(1 + q)) '(my-+ 1 q))
   (check-equal? (parse '(1 + q - 2)) '(my-+ 1 (my-- q 2))))

  (test-case
   "order of operations"
   (check-equal? (parse '(1 * q - 2)) '(my-- (my-* 1 q) 2))
   (check-equal? (parse '(1 - q * 2)) '(my-- 1 (my-* q 2))))

  (test-case
   "parens"
   (check-equal? (parse '( < 1 - q  > * 2)) '(my-* (my-- 1 q) 2))
   (check-equal? (parse '( < 1 - q  > * < 2 > )) '(my-* (my-- 1 q) 2))))
  


(display "Part 1 tests:\n");
(run-tests part1-tests)

(display "Part 2 tests:\n");
(run-tests part2-tests)

; if you would like to see the results in a GUI, you can run this (I find the gui
; annoying, but feel free)

; (test/gui test-to-run)