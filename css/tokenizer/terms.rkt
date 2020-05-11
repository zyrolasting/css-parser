#lang racket/base

(provide (all-defined-out))

(define (surrogate-code-point? code-point)
  (and (>= code-point #xD800)
       (<= code-point #xDFFF)))

(define (hex-digit? ch)
  (and (char? ch)
       (or (and (char>=? ch #\A)
                (char<=? ch #\F))
           (and (char>=? ch #\a)
                (char<=? ch #\f))
           (digit? ch))))

(define (digit? ch)
  (and (char? ch)
       (char>=? ch #\0)
       (char<=? ch #\9)))
