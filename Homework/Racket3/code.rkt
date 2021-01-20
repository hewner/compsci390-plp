#lang racket

;;
;;
;; PART 1
;;
;; So we're going to use continutaions to build a very simple exception
;; system in scheme.  Here's the the basic idea which initially is very similar
;; to Java/C++.

;;    (try (lambda () (+ 1 3))
;;         (lambda (error) (display "an error")))))

;; Try is a function that takes 2 lambdas as a parameter.
;;
;; The first is basically the try-code you want to run.  The second is basically the
;; code you want to run on error (i.e. the catch block).
;;
;; Try executes the try-code.  If nothing bad happens, it returns the result of the
;; try-code.  In this case, the second lambda is not used at all.
;;
;; If on the other hand some error is detected in the try lambda, the code should call
;; the special function error-abort.  error-abort should only be called from within the
;; try function (and you don't need to worry about what ought to happen if the user
;; violates that rule).  error-abort takes one parameter which can be anything (could be
;; an error code or error string).
;;
;; When error abort is called, the try code immediately stops running and the catch block
;; lambda is run.  The catch lambda should take one parameter - this parameter is whatever
;; was passed to error-abort.  The catch lambda can contain any code, but critically when
;; it finishes it's return result is the return of the entire try function.  Any code that
;; would have been executed afterwards in the try-code never executes.  So for example:


;;(try (lambda ()
;;       (display "a")
;;       (display "b")
;;       (error-abort "???")
;;       (display "c")
;;       0
;;       )
;;     (lambda (error) (display error) 99))

;; This code prints ab??? and returns 99.  The fact that c should not be printed is why you'll
;; need continuations.  A few things to make your life easier:
;;
;; 1.  You can use global (i.e. defined) variables and don't need to worry that your might
;; clobber some definition the user might want.  You can also use set! as much as you desire.
;; 2.  You do not need to worry about nested try blocks.  This simplifies things even though it's
;; not realistic - but the basic idea sound we'd just have to be more careful if we wanted nesting
;; to be possible.
;;
;; PART 1 - as described is worth 12 points.  It is checked by basic-throw-tests.


(define error-abort
  (lambda (error-val)
    'not-yet-implemented))

(define try
  (lambda (code-to-run exception-code)
    'not-yet-implemented))


;; PART 2

;; So here we would like to add an additional feature - the ability to resume from exceptions.
;; Now, if desired within a catch block a function error-resume can be called.  This resumes the
;; at the place the error-abort occured, allowing it to continue.  You can assume error-resume
;; will only be called within catch blocks.  You can also assume trys will not be nested so there's
;; not danger of multiple different resumes as the same time.
;;
;; Here's an example:

;;(try (lambda ()
;;       (display "a")
;;       (display "b")
;;       (error-abort "???")
;;       (display "c")
;;       0
;;       )
;;     (lambda (error) (display error) (error-resume) (display "never get here")))

;; This code will print ab???c and return 0.  "never get here will" not print, because resume returns to the
;; try block and never continutes executing the catch block.
;;
;; A try block may error and resume several times - in each case resuming to the most recent error-abort.
;; A catch block is always executed from the beginning however - it does not continue from the most recent
;; error-resume (see the resume-multiple-times test case for an example)
;;
;; PART 2 - as described is worth 12 points.  It is checked by resume-tests.


(define error-resume
  (lambda ()
    'not-yet-implemented))


;; PART 3 - Macro
;;
;; Let's add a macro try-m that simplifies how we declare trys
;;
;; try-m works like this:

;; (try-m (error-abort 5) (x) (+ x 3))
;;
;; try-m expands to the usual form of try with 2 lambdas as parameters
;;
;; You're welcome to use either old style macros we discussed in lecture or the
;; new style that's talked about here https://docs.racket-lang.org/guide/pattern-macros.html
;;
;; if we wanted to be fancier we could get even more java like and say something
;; like (try-m (error-abort 5) catch (x) (+ x 3)) but its slightly more complicated
;; to build the macro that way so I'm being nice
;;
;; This part is worth 6 points


(require compatibility/defmacro)

(defmacro try-m body
  '(quote not-yet-implemented)
  )




(provide try error-abort error-resume try-m)

