#lang racket

;; This is a little live coded interpreter I made
;; for lecture 02a.  You don't have to use it for anything
;; ...just an example for your amusement.

(define eval-bool
  (lambda (exp env)
    (display "eval-bool")
    (display exp)
    (display "\n")
    (cond
      ((eq? exp #t) #t)
      ((eq? exp #f) #f)
      ((symbol? exp)
       (env-lookup exp env))
      (#t 
       (let ((operator (car exp)))
         (cond
           ((eq? operator 'and)
            (cond
              ((eq? (eval-bool (second exp) env) #f) #f)
              ((eq? (eval-bool (third exp) env) #f) #f)
              (#t #t)))
           ((eq? operator 'or)
            (cond
              ((eq? (eval-bool (second exp) env) #t) #t)
              ((eq? (eval-bool (third exp) env) #t) #t)
              (#t #f)))
           (#t 'error)
      ))))))

(define env-lookup
  (lambda (value env)
    (cond
      ((null? env) 'lookup-failed)
      ((eq? (caar env) value)
       (begin
         (display "returning ")
         (display (cdar env))
         (display " for ")
         (display value)
         (cdar env))
       )
      (#t (env-lookup value (cdr env))))))
                   

;; (eval-bool '(and #t #f))
(eval-bool '(or #f (and #t #t)) '())



(eval-bool '(or a (and #t b)) '((a . #f) (b . #f)))