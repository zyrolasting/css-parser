#lang racket/base

(provide (struct-out exn:fail:css:parse)
         (struct-out exn:fail:css:syntax)
         make-css3-parse-error
         make-css3-syntax-error
         maybe-raise
         strict?
         maybe-raise-css3-parse-error
         maybe-raise-css3-syntax-error)

(require "tokenizer/tokens.rkt")

(struct exn:fail:css:parse  exn:fail (line column))
(struct exn:fail:css:syntax exn:fail (line column))

(define strict? (make-parameter #t))

(define (maybe-raise v)
  (when (strict?)
    (raise v)))

(define (make-css3-parse-error in msg)
  (let-values ([(line col pos) (port-next-location in)])
    (exn:fail:css:parse (format "(~a:~a) ~a" (or line "?") (or col "?") msg)
                        (current-continuation-marks)
                        line
                        col)))

(define (make-css3-syntax-error tok msg)
  (exn:fail:css:syntax msg
                       (current-continuation-marks)
                       (token-line tok)
                       (token-column tok)))

(define (maybe-raise-css3-parse-error in msg)
  (maybe-raise (make-css3-parse-error in msg)))

(define (maybe-raise-css3-syntax-error tok msg)
  (maybe-raise (make-css3-syntax-error tok msg)))
