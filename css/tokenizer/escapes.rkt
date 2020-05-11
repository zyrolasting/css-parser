#lang racket/base

(provide read-escaped
         valid-escape?)

(require "terms.rkt"
         "../exn.rkt"
         "../preprocess.rkt")

; §4.3.7
(define (read-escaped in)
  (let ([next (read-char/css in)])
    (cond [(eof-object? next)
           #\uFFFD]
          [(hex-digit? next)
           (define err (make-parse-error "Too many hex digits"))
           (define hex
             (string->number
              (let loop ([digits (list next)] [succ (peek-char in)])
                (if (> (length digits) 6)
                    (raise err)
                    (if (hex-digit? succ)
                        (begin
                          (read-char in)
                          (loop (cons succ digits)
                                (peek-char in)))
                        (apply string (reverse digits)))))))

           (when (char-whitespace? (peek-char/css in))
             (read-char/css in))

           (if (or (eq? hex 0)
                   (surrogate-code-point? hex)
                   (> hex #x10FFFF))
               #\uFFFD
               (integer->char hex))]

          [else next])))

; §4.3.8
(define valid-escape?
  (case-lambda
    [(in) (apply valid-escape?
                 (peek-char/css/multi in 2))]
    [(prev next)
     (and (char=? prev #\u005C)
          (not (char=? next #\u000A)))]))
