#lang racket/base
 
(require rackunit
         rackunit/text-ui
         rackunit/gui
         "code.rkt")
(define-test-suite reject-tests
  (test-equal? "reject 1" (reject null? '(a () () b () c)) '(a b c))
  (test-equal? "reject 2" (reject (lambda (n) (> n 3)) '(6 3 2 4 6 7 1)) '(3 2 1)))

(define-test-suite adds-tests
  (test-equal? "func-that-adds 1" ((func-that-adds 3) 3) 6)
  (test-equal? "func-that-adds 2" (map (func-that-adds 2) '(1 2 3)) '(3 4 5)))

(define-test-suite listify-tests
  (test-equal? "listify 1" ((listify-func (lambda (n) (+ n 3))) '(1 2 3 4)) '(4 5 6 7))
  (test-equal? "listify 2" ((listify-func null?) '(() (a b) (c) ())) '(#t #f #f #t)))

(define-test-suite chain-tests
  (test-equal? "chain 1" ((chain-func (list cdr length)) '(a b c)) 2)
  (test-equal? "chain 2" ((chain-func (list car cdr cdr car)) '((a b c d) e f g)) 'c))

(define-test-suite all-tests reject-tests adds-tests listify-tests chain-tests)

; this runs all the tests

(run-tests all-tests)

