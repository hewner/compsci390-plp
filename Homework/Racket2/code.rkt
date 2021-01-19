#lang racket/base

; This exercise has 4 problems, each worth 6 points for a total of 24 points.

; for all these problems, use recursion not iteration
; do not modify the values of variables (e.g. set methods disallowed)

; Write a function "reject" that takes 2 parameters: a function (i.e. lambda) and a list
;
; The function parameter takes a list item and returns true or false
; reject returns a new list where all the members that the function returns true on have been removed
;
; There is a scheme library function that does pretty much this - but don't use it, implement it from scracth
;
; Examples:
; (reject null? '(a () () b () c)) => (a b c)
; (reject (lambda (n) (> n 3)) '(6 3 2 4 6 7 1)) => (3 2 1)

(define reject
  (lambda (func list)
    'not-yet-implemented))

; Write a function "func-that-adds" that takes an integer and returns a 1 parameter
; function that adds that integer to its input
;
; Examples
; ((func-that-adds 3) 3) => 6
; (map (func-that-adds 2) '(1 2 3)) => (3 4 5)

(define func-that-adds
  (lambda (num)
    'not-yet-implemented))

; Write a function listify-func which takes a function that takes 1 parameter
; 
; listify-func should return a new function which takes a list as a parameter
; when passed a list, the new function should apply the original function to
; each member of the list and return a list as a result
;
; Hint: use map within your lambda
(define listify-func
  (lambda (func)
    'not-yet-implemented))

; Example
; (define (add3 n) (+ 3 n))
; ((listify-func add3) '(1 2 3 4)) => (4 5 6 7)

; Write a function chain-func which combines a list of one parameter functions into one
; function.  The output of the first step of the chain should be passed as the input
; to the second step etc.
(define chain-func
  (lambda (lst)
    'not-yet-implemented))

; Examples:
; ((chain-func (list cdr length)) '(a b c)) => 2
; ((chain-func (list (func-that-adds 1) (func-that-adds 2) (func-that-adds 3))) 2) => 8


(provide reject func-that-adds listify-func chain-func)
      