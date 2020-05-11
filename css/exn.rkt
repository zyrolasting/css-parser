#lang racket/base

(provide (struct-out exn:fail:css:parse)
         make-parse-error
         raise-parse-error)

(struct exn:fail:css:parse exn:fail (line col))

(define (make-parse-error in msg)
  (let-values ([(line col pos) (port-next-location in)])
    (exn:fail:css:parse (format "(~a:~a) ~a" line col msg)
                        (current-continuation-marks)
                        line
                        col)))


(define (raise-parse-error in msg)
  (raise (make-parse-error in msg)))
