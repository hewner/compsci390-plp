#lang racket/base
 
(require rackunit
         "code.rkt")

(check-equal? (shorter '(a b) '(c)) '(c) "shorter 1")
(check-equal? (shorter '(a) '(b c)) '(a) "shorter 2")
(check-equal? (shorter '(a) '(b)) '(a) "shorter equal length")

(check-equal? (make-list 3) '(() () ()) "make list 3")
(check-equal? (make-list 5) '(() () () () ()) "make list 5")
(check-equal? (make-list 1) '(()) "make list 1")
(check-equal? (make-list 0) '() "make list 5")

(check-equal? (list-ref '(1 2 3 4) 0) 1)
(check-equal? (list-tail '(1 2 3 4) 0) '(1 2 3 4))
(check-equal? (list-ref '(a short (nested) list) 2) '(nested))
(check-equal? (list-tail '(a short (nested) list) 2) '((nested) list))

(check-equal? (transpose '((a . 1) (b . 2) (c . 3))) '((a b c) 1 2 3) "basic transpose")
(check-equal? (transpose '((a . 1) (b . 2) (c . 3) (d . 4))) '((a b c d) 1 2 3 4) "basic transpose 2")

(display "tests done")