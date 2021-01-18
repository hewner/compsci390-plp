#lang racket/base

; if you look at the bottom of this file you can see how tests are invoked

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "code.rkt")

(define-test-suite shorter-tests
 (test-equal? "xyz" (shorter '(a b) '(c)) '(c) )
 (test-equal? "shorter 2" (shorter '(a) '(b c)) '(a))
 (test-equal? "shorter equal length" (shorter '(a) '(b)) '(a) ))

(define-test-suite make-list-tests
  (test-equal? "make list 3" (make-list 3 '()) '(() () ()))
  (test-equal? "make list 5" (make-list 5 'x) '(x x x x x))
  (test-equal? "make list 1" (make-list 1 '()) '(()) )
  (test-equal? "make list 5" (make-list 0 3) '()))

(define-test-suite list-ref-tail-tests
  (test-equal? "ref1" (list-ref '(1 2 3 4) 0) 1)
  (test-equal? "tail1" (list-tail '(1 2 3 4) 0) '(1 2 3 4))
  (test-equal? "ref2" (list-ref '(a short (nested) list) 2) '(nested))
  (test-equal? "tail2" (list-tail '(a short (nested) list) 2) '((nested) list)))

(define-test-suite transpose-tests
  (test-equal? "basic transpose" (transpose '((a . 1) (b . 2) (c . 3))) '((a b c) 1 2 3))
  (test-equal?  "basic transpose 2" (transpose '((a . 1) (b . 2) (c . 3) (d . 4))) '((a b c d) 1 2 3 4)))

(define-test-suite all-tests shorter-tests make-list-tests list-ref-tail-tests transpose-tests)

; this runs all the tests

(run-tests all-tests)

; if you would like to just test one individual suite you can do it like this:
; (run-tests make-list-tests)

; if you would like to see the results in a GUI, you can run this (I find the gui
; annoying, but feel free)

; (test/gui all-tests)