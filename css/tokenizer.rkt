#lang racket/base

(provide get-next-token
         tokenize)

(require "preprocess.rkt"
         "exn.rkt"
         "tokenizer/terms.rkt"
         "tokenizer/tokens.rkt"
         "tokenizer/identifiers.rkt"
         "tokenizer/names.rkt"
         "tokenizer/escapes.rkt"
         "tokenizer/numbers.rkt"
         "tokenizer/comments.rkt")

; §4
(define (tokenize in [out null])
  (let ([next (get-next-token in)])
    (if (eof-token? next)
        (reverse (cons next out))
        (tokenize in
                  (if (void? next)
                      out
                      (cons next out))))))

; §4.3.1
(define (get-next-token in)
  (define next (peek-char/css in))
  (cond [(eof-object? next) (eof-token)]
        [(starts-comment? in) (consume-comment in)]
        [(whitespace? next) (make-whitespace-token in)]
        [(char=? next #\u0022) (consume-string-token in (read-char in) null)]
        [(char=? next #\u0027) (consume-string-token in (read-char in) null)]
        [(char=? next #\u0023) (consume-hash-or-delim-token in)]
        [(char=? next #\u002B) (make-numeric-or-delim-token in)]
        [(char=? next #\u0028) (l-paren-token)]
        [(char=? next #\u0029) (r-paren-token)]
        [(char=? next #\u002C) (comma-token)]
        [(char=? next #\u003A) (colon-token)]
        [(char=? next #\u003B) (semicolon-token)]
        [(char=? next #\u005B) (l-square-bracket-token)]
        [(char=? next #\u005D) (r-square-bracket-token)]
        [(char=? next #\u007B) (l-curly-bracket-token)]
        [(char=? next #\u007D) (r-curly-bracket-token)]
        [else (delim-token next)]))


(define (consume-hash-or-delim-token in)
  (read-char in) ; Discard #
  (if (or (name-code-point? (peek-char/css in))
          (valid-escape? in))
      (hash-token (if (starts-identifier? in) 'id #f)
                  (consume-name in))
      (delim-token (read-char/css in))))

(define (make-whitespace-token in [next (peek-char/css in)])
  (if (whitespace? next)
      (make-whitespace-token in (begin (read-char/css in)
                                       (peek-char/css in)))
      (whitespace-token)))

(define (make-numeric-or-delim-token in)
  (read-char in) ; Discard + or -
  (if (starts-number? in)
      (consume-numeric in)
      (delim-token (peek-char/css in))))


; §4.3.5
(define (consume-string-token in ending-char [chars null])
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
                    (consume-string-token in ending-char chars)]

                   [(char=? escaped #\u000A)
                    ; Escaped newline. Consume the newline and continue
                    ; as if the next line is part of the literal.
                    (read-char/css in)
                    (consume-string-token in ending-char chars)]

                   [else
                    (consume-string-token
                     in ending-char
                     (cons (read-escaped in) chars))]))]

          [else
           (consume-string-token
            in ending-char (cons (read-char/css in) chars))])))
