#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "code.rkt")

(define-test-suite basic-throw-tests
  (test-equal? "no error"
               (try
                (lambda () (+ 1 3))
                (lambda (error) 999)) 4)
  (test-equal? "error1"
               (try
                (lambda () (error-abort 123) 456)
                (lambda (error) 999)) 999)
  (test-equal? "error2"
               (try
                (lambda () (error-abort 123) 456)
                (lambda (error) (+ 1 error))) 124)
  (test-equal? "don't execute more after abort"
               (let ((x 5)) (try
                (lambda () (error-abort 123) (set! x 6))
                (lambda (error) (+ 1 error)))
                 x)
                 5))

(define-test-suite resume-tests
  (test-equal? "simple resume"
               (try
                (lambda () (error-abort 99) 4)
                (lambda (error) (error-resume) 500)) 4)
  (test-equal? "don't finish catch after resume"
               (try
                (lambda () (error-abort 99) 4)
                (lambda (error) (error-resume) (null))) 4)
  (test-equal? "resume multiple times"
               (try
                (lambda () (error-abort 99) (error-abort 100) 5)
                (lambda (error) (error-resume) 777)) 5)

  )


(define-test-suite macro-tests
  (test-equal? "no error macro"
               (try-m
                (+ 1 3)
                (x) x)
                4)

  (test-equal? "no error macro"
               (try-m
                (error-abort 5)
                (x) (+ x 3))
                8)
  )

(define-test-suite all-tests basic-throw-tests resume-tests macro-tests)

; this runs all the tests

(run-tests all-tests)

