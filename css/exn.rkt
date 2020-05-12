#lang racket/base

(provide (struct-out exn:fail:css:parse)
         make-parse-error
         maybe-raise
         strict?
         raise-parse-error)

(struct exn:fail:css:parse exn:fail (line col))

(define strict? (make-parameter #t))

(define (make-parse-error in msg)
  (let-values ([(line col pos) (port-next-location in)])
    (exn:fail:css:parse (format "(~a:~a) ~a" (or line "?") (or col "?") msg)
                        (current-continuation-marks)
                        line
                        col)))

(define (maybe-raise v)
  (when (strict?)
    (raise v)))

(define (raise-parse-error in msg)
  (maybe-raise (make-parse-error in msg)))
