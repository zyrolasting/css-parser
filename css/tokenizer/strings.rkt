#lang racket/base

(provide starts-string?
         make-string-token)

(require "terms.rkt"
         "tokens.rkt"
         "escapes.rkt"
         "../exn.rkt"
         "../preprocess.rkt")

(define (starts-string? ch)
  (and (char? ch)
       (or (char=? ch #\u0022)
           (char=? ch #\u0027))))

; String tokens (§4.3.5)
(define (make-string-token in ending-char [chars null])
  (let ([next (peek-char/css in)])
    (cond [(eof-object? next)
           (raise-parse-error in "Unexpected EOF when parsing string.")]

          [(char=? next ending-char)
           (read-char/css in) ; Consume closing quote.
           (string-token (apply string (reverse chars)))]

          [(char=? next #\u000A) ; Unescaped newline within string literal.
           (bad-string-token)]

          ; Process escaped characters
          [(char=? next #\u005C)
           (read-char/css in) ; Consume the '\'
           (let ([escaped (peek-char/css in)])
             (cond [(eof-object? escaped)
                    ; Lead into error case.
                    (make-string-token in ending-char chars)]

                   [(char=? escaped #\u000A)
                    ; Escaped newline. Consume the newline and continue
                    ; as if the next line is part of the literal.
                    (read-char/css in)
                    (make-string-token in ending-char chars)]

                   [else
                    (make-string-token
                     in ending-char
                     (cons (read-escaped in) chars))]))]

          [else
           (make-string-token
            in ending-char (cons (read-char/css in) chars))])))
