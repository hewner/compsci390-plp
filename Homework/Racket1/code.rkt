#lang racket/base

; Skim/read chapter 2 here:
; https://scheme.com/tspl4/start.html

; for all these problems, use recursion not iteration
; do not modify the values of variables (e.g. set methods disallowed)

; To test - as you develop it's probably easiest to just write the functions
; above, hit run and then evaluate test expressions below.
;
; But you should also test against the included unit tests.  Take in tests.rkt
; to see the tests and how to run them

; There are 4 exercises here - each is worth 6 points for a total of 24 points.


; Exercise 2.7.2
; The procedure length returns the length of its argument, which must be a list. For example, (length '(a b c)) is 3.
; Using length, define the procedure shorter, which returns the shorter of two list arguments. Have it return the
; first list if they have the same length.
;
; examples:
;
; (shorter '(a b) '(c d e)) => (a b)
; (shorter '(a b) '(c d)) => (a b)
; (shorter '(a b) '(c)) => (c)


(define shorter
  (lambda (list1 list2)
    'not-yet-implemented))


; Exercise 2.8.3
;
; Define the procedure make-list, which takes a nonnegative integer n and an object and returns a new list, n long, each
; element of which is the object.
;
; Example
; (make-list 7 '()) => (() () () () () () ())
;
; there's a hint in the chapter if you need it

(define make-list
  (lambda (size value)
    'not-yet-implemented))
      

; Exercise 2.8.4
; The procedures list-ref and list-tail return the nth element and nth tail of a list ls.

; (list-ref '(1 2 3 4) 0) => 1
; (list-tail '(1 2 3 4) 0) => (1 2 3 4)
; (list-ref '(a short (nested) list) 2) => (nested)
; (list-tail '(a short (nested) list) 2) => ((nested) list) 

(define list-ref
  (lambda (lst num)
    'not-yet-implemented))

(define list-tail
  (lambda (lst num)
    'not-yet-implemented))

; Exercise 2.8.7
;
; Use map to define a procedure, transpose, that takes a list of pairs and returns a pair of lists as follows.
;
; (transpose '((a . 1) (b . 2) (c . 3))) => ((a b c) 1 2 3) 
;
; [Hint: ((a b c) 1 2 3) is the same as ((a b c) . (1 2 3)).] 

(define transpose
  (lambda (pair-list)
    'not-yet-implemented))

(provide shorter make-list list-ref list-tail transpose)