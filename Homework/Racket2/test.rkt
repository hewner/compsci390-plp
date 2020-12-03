#lang racket/base
 
(require rackunit
         "code.rkt")

(check-equal? (reject null? '(a () () b () c)) '(a b c))
(check-equal? (reject (lambda (n) (> n 3)) '(6 3 2 4 6 7 1)) '(3 2 1))

(check-equal? ((func-that-adds 3) 3) 6)
(check-equal? (map (func-that-adds 2) '(1 2 3)) '(3 4 5))

(check-equal? ((listify-func (lambda (n) (+ n 3))) '(1 2 3 4)) '(4 5 6 7))
(check-equal? ((listify-func null?) '(() (a b) (c) ())) '(#t #f #f #t))

(check-equal? ((chain-func (list cdr length)) '(a b c)) 2)
(check-equal? ((chain-func (list car cdr cdr car)) '((a b c d) e f g)) 'c)

(display "tests done")